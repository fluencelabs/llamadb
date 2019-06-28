use super::sexpression::{BinaryOp, SExpression, UnaryOp};
use columnvalueops::{ColumnValueOps, ColumnValueOpsExt};
use databaseinfo::DatabaseInfo;
use databasestorage::{DatabaseStorage, Group};

mod aggregate;
use self::aggregate::*;

mod groupbuckets;
use self::groupbuckets::GroupBuckets;
use queryplan::QueryPlanCompileError::NotImplemented;
use tempdb::ExecuteError;

enum SourceType<'a, ColumnValue: Sized + 'static> {
    Row(&'a [ColumnValue]),
    RawRow(&'a [ColumnValue], &'a Vec<u8>),
    Group(&'a Group<ColumnValue = ColumnValue>),
}

struct Source<'a, ColumnValue: Sized + 'static> {
    parent: Option<&'a Source<'a, ColumnValue>>,
    source_id: u32,
    source_type: SourceType<'a, ColumnValue>,
}

impl<'a, ColumnValue: Sized> Source<'a, ColumnValue> {
    /// Finds row by specified 'source_id'
    fn find_row_from_source_id(&self, source_id: u32) -> Option<&[ColumnValue]> {
        if self.source_id == source_id {
            match &self.source_type {
                &SourceType::Row(row) | &SourceType::RawRow(row, _) => Some(row),
                _ => None,
            }
        } else if let Some(parent) = self.parent {
            parent.find_row_from_source_id(source_id)
        } else {
            None
        }
    }

    /// Finds group by specified 'source_id'
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

    /// Executes specified query plan. Call `row_cb` and `raw_row_cb` for each
    /// row in result.
    ///
    /// * `expr` - query plan for execute
    /// * `row_cb` - this callback will call for each row in the result of the
    ///             query where row is array of columns
    /// * `raw_row_cb` - this callback will call for each row in the result of
    ///                 the query where row is raw byte array
    ///
    pub fn execute_query_plan<'b, 'c>(
        &self,
        expr: &SExpression<'a, Storage::Info>,
        row_cb: &'c mut FnMut(
            &[<Storage::Info as DatabaseInfo>::ColumnValue],
        ) -> Result<(), ExecuteError>,
        raw_row_cb: &'c mut FnMut(&Vec<u8>) -> Result<(), ExecuteError>,
    ) -> Result<(), ExecuteError> {
        self.execute(expr, row_cb, None, raw_row_cb)
        // TODO: row_cb should yield a boxed array instead of a reference
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
        row_cb: &'c mut FnMut(
            &[<Storage::Info as DatabaseInfo>::ColumnValue],
        ) -> Result<(), ExecuteError>,
        source: Option<&Source<'b, <Storage::Info as DatabaseInfo>::ColumnValue>>,
        raw_row_cb: &'c mut FnMut(&Vec<u8>) -> Result<(), ExecuteError>,
    ) -> Result<(), ExecuteError> {
        match expr {
            &SExpression::Scan {
                table,
                source_id,
                ref yield_fn,
            } => {
                let group = self.storage.scan_table(table);
                for row in group.iter_raw() {
                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::RawRow(row.row.as_ref(), row.raw_row),
                    };

                    self.execute(yield_fn, row_cb, Some(&new_source), raw_row_cb)?;
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
                            self.execute(yield_out_fn, row_cb, Some(&new_source), &mut |_| Ok(()))
                        } else {
                            Ok(())
                        }
                    },
                    source,
                    raw_row_cb,
                )?;

                if !one_or_more_rows {
                    // no rows were matched
                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Row(right_rows_if_none),
                    };

                    self.execute(yield_out_fn, row_cb, Some(&new_source), raw_row_cb)
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

                    self.execute(yield_out_fn, row_cb, Some(&new_source), &mut |_| Ok(()))
                },
                source,
                raw_row_cb,
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
                    raw_row_cb,
                )?;

                // the group buckets have been filled.
                // now to yield for each group...

                for group in group_buckets {
                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Group(&group),
                    };

                    self.execute(yield_out_fn, row_cb, Some(&new_source), raw_row_cb)?;
                }

                Ok(())
            },
            &SExpression::Yield { ref fields } => {
                let columns: Result<Vec<_>, _> = fields
                    .iter()
                    .map(|e| self.resolve_value(e, source))
                    .collect();

                let src: &Source<_> = source.unwrap();
                match src.source_type {
                    SourceType::RawRow(_, raw_row) => {
                        raw_row_cb(raw_row)?;
                    },
                    _ => {}, // do nothing
                }

                columns.map(|columns| row_cb(&columns))?
            },
            &SExpression::If {
                ref chains,
                ref else_,
            } => {
                for chain in chains {
                    let pred_result = self.resolve_value(&chain.predicate, source)?;

                    if pred_result.tests_true() {
                        return self.execute(&chain.yield_fn, row_cb, source, raw_row_cb);
                    }
                }

                if let Some(e) = else_.as_ref() {
                    self.execute(e, row_cb, source, raw_row_cb)
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

                match op {
                    BinaryOp::Equal => Ok(l.equals(&r).map_err(ExecuteError::from_string)?),
                    BinaryOp::NotEqual => {
                        Ok(l.not_equals(&r).map_err(ExecuteError::from_string)?)
                    },
                    BinaryOp::LessThan => Ok(l.less_than(&r).map_err(ExecuteError::from_string)?),
                    BinaryOp::LessThanOrEqual => Ok(l
                        .less_than_or_equal(&r)
                        .map_err(ExecuteError::from_string)?),
                    BinaryOp::GreaterThan => {
                        Ok(l.greater_than(&r).map_err(ExecuteError::from_string)?)
                    },
                    BinaryOp::GreaterThanOrEqual => Ok(l
                        .greater_than_or_equal(&r)
                        .map_err(ExecuteError::from_string)?),
                    BinaryOp::And => Ok(l.and(&r)),
                    BinaryOp::Or => Ok(l.or(&r)),
                    BinaryOp::Concatenate => Ok(l.concat(&r).map_err(ExecuteError::from_string)?),
                    BinaryOp::Add => Ok(l.add(&r).map_err(ExecuteError::from_string)?),
                    BinaryOp::Subtract => Ok(l.sub(&r).map_err(ExecuteError::from_string)?),
                    BinaryOp::Multiply => Ok(l.mul(&r).map_err(ExecuteError::from_string)?),
                    BinaryOp::Divide => Ok(l.div(&r).map_err(ExecuteError::from_string)?),
                    op => Err(ExecuteError::from(NotImplemented(op.to_string()))),
                }
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
                            op_functor.feed(v).map_err(ExecuteError::from_string)?;
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
                let mut result_row = None;
                let mut row_count = 0;

                self.execute(
                    yield_in_fn,
                    &mut |row| {
                        if row_count == 0 {
                            result_row = Some(row.to_vec());
                        }
                        row_count += 1;
                        Ok(())
                    },
                    source,
                    &mut |_| Ok(()),
                )?;

                if row_count == 1 {
                    let row = result_row
                        .ok_or(ExecuteError::new("subquery must yield exactly one row"))?;

                    let new_source = Source {
                        parent: source,
                        source_id,
                        source_type: SourceType::Row(&row),
                    };

                    self.resolve_value(yield_out_fn, Some(&new_source))
                } else {
                    Err(ExecuteError::new("subquery must yield exactly one row"))
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

// todo write tests
