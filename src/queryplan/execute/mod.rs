use super::sexpression::{BinaryOp, SExpression, UnaryOp};
use columnvalueops::{ColumnValueOps, ColumnValueOpsExt};
use databaseinfo::DatabaseInfo;
use databasestorage::{DatabaseStorage, Group};

mod aggregate;
use self::aggregate::*;

mod groupbuckets;
use self::groupbuckets::GroupBuckets;
use tempdb::ExecuteError;

enum SourceType<'a, ColumnValue: Sized + 'static> {
    Row(&'a [ColumnValue]),
    Group(&'a Group<ColumnValue = ColumnValue>),
}

struct Source<'a, ColumnValue: Sized + 'static> {
    parent: Option<&'a Source<'a, ColumnValue>>,
    source_id: u32,
    source_type: SourceType<'a, ColumnValue>,
}

impl<'a, ColumnValue: Sized> Source<'a, ColumnValue> {
    fn find_row_from_source_id(&self, source_id: u32) -> Option<&[ColumnValue]> {
        if self.source_id == source_id {
            match &self.source_type {
                &SourceType::Row(row) => Some(row),
                _ => None,
            }
        } else if let Some(parent) = self.parent {
            parent.find_row_from_source_id(source_id)
        } else {
            None
        }
    }

    fn find_group_from_source_id(
        &self,
        source_id: u32,
    ) -> Option<&Group<ColumnValue = ColumnValue>> {
        if self.source_id == source_id {
            match &self.source_type {
                &SourceType::Group(group) => Some(group),
                _ => None,
            }
        } else if let Some(parent) = self.parent {
            parent.find_group_from_source_id(source_id)
        } else {
            None
        }
    }
}

/// The query plan is currently defined as a recursive language.
/// Because of this, it would take some work (and foresight) to make query plan
/// execution co-operate with the concept of iterators.
/// For now, callbacks are used to yield rows.
///
/// TODO: translate query plan into procedural language
/// (such as VM instructions, like those found in SQLite's VBDE).
pub struct ExecuteQueryPlan<'s, Storage: DatabaseStorage + 's> {
    storage: &'s Storage,
}

impl<'a, 's, Storage: DatabaseStorage> ExecuteQueryPlan<'s, Storage>
where
    <Storage::Info as DatabaseInfo>::Table: 'a,
{
    pub fn new(storage: &'s Storage) -> ExecuteQueryPlan<'s, Storage> {
        ExecuteQueryPlan { storage }
    }

    // TODO: result_cb should yield a boxed array instead of a reference
    pub fn execute_query_plan<'b, 'c>(
        &self,
        expr: &SExpression<'a, Storage::Info>,
        result_cb: &'c mut FnMut(&[<Storage::Info as DatabaseInfo>::ColumnValue])
            -> Result<(), ExecuteError>,
    ) -> Result<(), ExecuteError> {
        self.execute(expr, result_cb, None)
    }

    pub fn execute_expression(
        &self,
        expr: &SExpression<'a, Storage::Info>,
    ) -> Result<<Storage::Info as DatabaseInfo>::ColumnValue, ExecuteError> {
        self.resolve_value(expr, None)
    }

    fn execute<'b, 'c>(
        &self,
        expr: &SExpression<'a, Storage::Info>,
        result_cb: &'c mut FnMut(&[<Storage::Info as DatabaseInfo>::ColumnValue])
            -> Result<(), ExecuteError>,
        source: Option<&Source<'b, <Storage::Info as DatabaseInfo>::ColumnValue>>,
    ) -> Result<(), ExecuteError> {
        match expr {
            &SExpression::Scan {
                table,
                source_id,
                ref yield_fn,
            } => {
                let group = self.storage.scan_table(table);
                for row in group.iter() {
                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Row(&row),
                    };

                    self.execute(yield_fn, result_cb, Some(&new_source))?;
                }

                Ok(())
            },
            &SExpression::LeftJoin {
                source_id,
                ref yield_in_fn,
                ref predicate,
                ref yield_out_fn,
                ref right_rows_if_none,
            } => {
                let mut one_or_more_rows = false;

                self.execute(
                    yield_in_fn,
                    &mut |row| {
                        let new_source = Source {
                            parent: source,
                            source_id,
                            source_type: SourceType::Row(row),
                        };

                        let pred_result = self.resolve_value(predicate, Some(&new_source))?;

                        if pred_result.tests_true() {
                            one_or_more_rows = true;
                            self.execute(yield_out_fn, result_cb, Some(&new_source))
                        } else {
                            Ok(())
                        }
                    },
                    source,
                )?;

                if !one_or_more_rows {
                    // no rows were matched
                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Row(right_rows_if_none),
                    };

                    self.execute(yield_out_fn, result_cb, Some(&new_source))
                } else {
                    Ok(())
                }
            },
            &SExpression::Map {
                source_id,
                ref yield_in_fn,
                ref yield_out_fn,
            } => self.execute(
                yield_in_fn,
                &mut |row| {
                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Row(row),
                    };

                    self.execute(yield_out_fn, result_cb, Some(&new_source))
                },
                source,
            ),
            &SExpression::TempGroupBy {
                source_id,
                ref yield_in_fn,
                ref group_by_values,
                ref yield_out_fn,
            } => {
                let mut group_buckets = GroupBuckets::new();

                self.execute(
                    yield_in_fn,
                    &mut |row| {
                        let new_source = Source {
                            parent: source,
                            source_id,
                            source_type: SourceType::Row(row),
                        };

                        let result: Result<Vec<_>, _> = group_by_values
                            .iter()
                            .map(|value| self.resolve_value(value, Some(&new_source)))
                            .collect();

                        let key = result?;

                        // TODO: don't box up row
                        let row_boxed = row.to_vec().into_boxed_slice();

                        group_buckets.insert(key.into_boxed_slice(), row_boxed);

                        Ok(())
                    },
                    source,
                )?;

                // the group buckets have been filled.
                // now to yield for each group...

                for group in group_buckets {
                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Group(&group),
                    };

                    self.execute(yield_out_fn, result_cb, Some(&new_source))?;
                }

                Ok(())
            },
            &SExpression::Yield { ref fields } => {
                let columns: Result<Vec<_>, _> = fields
                    .iter()
                    .map(|e| self.resolve_value(e, source))
                    .collect();

                columns.map(|columns| result_cb(&columns))?
            },
            &SExpression::If {
                ref chains,
                ref else_,
            } => {
                for chain in chains {
                    let pred_result = self.resolve_value(&chain.predicate, source)?;

                    if pred_result.tests_true() {
                        return self.execute(&chain.yield_fn, result_cb, source);
                    }
                }

                if let Some(e) = else_.as_ref() {
                    self.execute(e, result_cb, source)
                } else {
                    Ok(())
                }
            },
            &SExpression::ColumnField { .. } |
            &SExpression::BinaryOp { .. } |
            &SExpression::UnaryOp { .. } |
            &SExpression::AggregateOp { .. } |
            &SExpression::CountAll { .. } |
            &SExpression::Value(..) => Err(ExecuteError::from_string(format!(
                "encountered expression that cannot yield rows"
            ))),
        }
    }

    fn resolve_value<'b>(
        &self,
        expr: &SExpression<'a, Storage::Info>,
        source: Option<&Source<'b, <Storage::Info as DatabaseInfo>::ColumnValue>>,
    ) -> Result<<Storage::Info as DatabaseInfo>::ColumnValue, ExecuteError> {
        match expr {
            &SExpression::Value(ref v) => Ok(v.clone()),
            &SExpression::ColumnField {
                source_id,
                column_offset,
            } => {
                let row = source.and_then(|s| s.find_row_from_source_id(source_id));
                match row {
                    Some(row) => Ok(row[column_offset as usize].clone()),
                    None => {
                        // the source might actually be a group.
                        // in this case, any arbitrary row from the group is valid to yield.
                        match source.and_then(|s| s.find_group_from_source_id(source_id)) {
                            Some(group) => match group.get_any_row() {
                                Some(row) => Ok(row[column_offset as usize].clone()),
                                None => Ok(ColumnValueOpsExt::null()),
                            },
                            None => Err(ExecuteError::from_string(format!(
                                "ColumnField: source id is not a valid row or group: {}",
                                source_id
                            ))),
                        }
                    },
                }
            },
            &SExpression::BinaryOp {
                op,
                ref lhs,
                ref rhs,
            } => {
                let l = self.resolve_value(lhs, source)?;
                let r = self.resolve_value(rhs, source)?;

                Ok(match op {
                    BinaryOp::Equal => l.equals(&r),
                    BinaryOp::NotEqual => l.not_equals(&r),
                    BinaryOp::LessThan => l.less_than(&r),
                    BinaryOp::LessThanOrEqual => l.less_than_or_equal(&r),
                    BinaryOp::GreaterThan => l.greater_than(&r),
                    BinaryOp::GreaterThanOrEqual => l.greater_than_or_equal(&r),
                    BinaryOp::And => l.and(&r),
                    BinaryOp::Or => l.or(&r),
                    BinaryOp::Concatenate => l.concat(&r),
                    BinaryOp::Add => l.add(&r),
                    BinaryOp::Subtract => l.sub(&r),
                    BinaryOp::Multiply => l.mul(&r),
                    BinaryOp::Divide => l.div(&r),
                    _ => unimplemented!(),
                })
            },
            &SExpression::UnaryOp { op, ref expr } => {
                let e = self.resolve_value(expr, source)?;

                Ok(match op {
                    UnaryOp::Negate => e.negate(),
                })
            },
            &SExpression::AggregateOp {
                op,
                source_id,
                ref value,
            } => {
                let group = source.and_then(|s| s.find_group_from_source_id(source_id));
                match group {
                    Some(group) => {
                        let mut op_functor = get_aggregate_function(op);

                        for row in group.iter() {
                            let new_source = Source {
                                parent: source,
                                source_id,
                                source_type: SourceType::Row(&row),
                            };

                            let v = self.resolve_value(value, Some(&new_source))?;
                            op_functor.feed(v);
                        }

                        Ok(op_functor.finish())
                    },
                    None => Err(ExecuteError::from_string(format!(
                        "AggregateOp: source id is not a valid group: {}",
                        source_id
                    ))),
                }
            },
            &SExpression::CountAll { source_id } => {
                match source.and_then(|s| s.find_group_from_source_id(source_id)) {
                    Some(group) => {
                        let count = group.count();
                        Ok(ColumnValueOps::from_u64(count))
                    },
                    None => Err(ExecuteError::from_string(format!(
                        "CountAll: source id is not a valid group: {}",
                        source_id
                    ))),
                }
            },
            &SExpression::Map {
                source_id,
                ref yield_in_fn,
                ref yield_out_fn,
            } => {
                trace!("resolve_value; map {}", source_id);

                // yield_in_fn is expected to yield exactly one row
                // yield_out_fn is expected to return a single resolved value
                let mut r = None;
                let mut row_count = 0;

                self.execute(
                    yield_in_fn,
                    &mut |row| {
                        if row_count == 0 {
                            r = Some(row.to_vec());
                        }
                        row_count += 1;
                        Ok(())
                    },
                    source,
                )?;

                if row_count == 1 {
                    let row = r.unwrap();

                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Row(&row),
                    };

                    self.resolve_value(yield_out_fn, Some(&new_source))
                } else {
                    Err(ExecuteError::from_string(format!(
                        "subquery must yield exactly one row"
                    )))
                }
            },
            &SExpression::Scan { .. } |
            &SExpression::LeftJoin { .. } |
            &SExpression::TempGroupBy { .. } |
            &SExpression::Yield { .. } |
            &SExpression::If { .. } => Err(ExecuteError::from_string(format!(
                "encounted expression that cannot resolve to a single value"
            ))),
        }
    }
}
