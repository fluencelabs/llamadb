//! A temporary in-memory database meant to hold us over until the pager and
//! B+Tree modules are finalized.
//!
//! This module will be removed once the pager and B+Tree are functional.

use std::borrow::Cow;
use std::collections::BTreeSet;

use columnvalueops::{ColumnValueOps, ColumnValueOpsExt};
use databaseinfo::{ColumnInfo, DatabaseInfo, TableInfo};
use databasestorage::{DatabaseStorage, Group};
use identifier::Identifier;
use queryplan::{self, ExecuteQueryPlan, QueryPlan};
use sqlsyntax::ast;
use types::{DbType, Variant};

mod table;
use self::table::Table;
use databasestorage::RawRow;
use queryplan::QueryPlanCompileError;
use sqlsyntax;
use sqlsyntax::ast::TableOrSubquery;
use sqlsyntax::ast::UpdateField;
use sqlsyntax::ParseError;
use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::option::Option::None;
use std::option::Option::Some;
use tempdb::table::Column;
use tempdb::table::UpdateError;
use tempdb::ExecuteStatementResponse::Updated;

pub struct TempDb {
    tables: Vec<Table>,
}

pub enum ExecuteStatementResponse<'a> {
    Created,
    Dropped,
    Inserted(u64),
    Select {
        column_names: Box<[String]>,
        rows: Box<Iterator<Item = Box<[Variant]>> + 'a>,
    },
    Deleted(usize),
    Explain(String),
    Updated(usize),
}

#[derive(PartialEq, Debug)]
pub struct ExecuteError {
    message: String,
}

impl ExecuteError {
    pub fn new(message: &'static str) -> Self {
        ExecuteError {
            message: message.to_string(),
        }
    }
    pub fn from_string(message: String) -> Self {
        ExecuteError { message }
    }
}

impl From<QueryPlanCompileError> for ExecuteError {
    fn from(err: QueryPlanCompileError) -> Self {
        ExecuteError {
            message: err.to_string(),
        }
    }
}

impl From<ParseError> for ExecuteError {
    fn from(err: ParseError) -> Self {
        ExecuteError {
            message: err.to_string(),
        }
    }
}

impl From<UpdateError> for ExecuteError {
    fn from(err: UpdateError) -> Self {
        ExecuteError {
            message: err.to_string(),
        }
    }
}

impl Display for ExecuteError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.message)
    }
}

impl Error for ExecuteError {}

pub type ExecuteStatementResult<'a> = Result<ExecuteStatementResponse<'a>, ExecuteError>;

impl DatabaseInfo for TempDb {
    type Table = Table;
    type ColumnValue = Variant;

    fn find_table_by_name(&self, name: &Identifier) -> Option<&Table> {
        self.tables.iter().find(|t| &t.name == name)
    }
}

struct ScanGroup<'a> {
    table: &'a Table,
}

impl<'a> Group for ScanGroup<'a> {
    type ColumnValue = Variant;

    fn get_any_row(&self) -> Option<Cow<[Variant]>> {
        self.iter().nth(0)
    }

    fn get_any_raw_row(&self) -> Option<RawRow<<Self as Group>::ColumnValue>> {
        self.iter_raw().nth(0)
    }

    fn count(&self) -> u64 {
        self.table.rows_set.len() as u64
    }

    fn iter<'b>(&'b self) -> Box<Iterator<Item = Cow<'b, [Variant]>> + 'b> {
        let table = self.table;
        let columns: &'b [self::table::Column] = &table.columns;

        Box::new(table.rows_set.iter().map(move |key_v| {
            use byteutils;

            let row: &[u8] = &key_v;
            trace!("row: {:?}", row);

            let variable_lengths = table.read_data_length(row);

            trace!("variable lengths: {:?}", variable_lengths);

            let _rowid: u64 = byteutils::read_udbinteger(&row[0..8]);

            let mut variable_length_offset = 0;
            let mut key_offset = 8;

            let v: Vec<Variant> = columns
                .iter()
                .map(|column| {
                    let is_null = if column.nullable {
                        let flag = row[key_offset];
                        key_offset += 1;
                        flag != 0
                    } else {
                        false
                    };

                    if is_null {
                        ColumnValueOpsExt::null()
                    } else {
                        let size = match column.dbtype.get_fixed_length() {
                            Some(l) => l as usize,
                            None => {
                                let l = variable_lengths[variable_length_offset];
                                variable_length_offset += 1;
                                l as usize
                            },
                        };

                        let bytes = &row[key_offset..key_offset + size];

                        trace!("from bytes: {:?}, {:?}", column.dbtype, bytes);
                        let value =
                            ColumnValueOps::from_bytes(column.dbtype, bytes.into()).unwrap();
                        key_offset += size;
                        value
                    }
                })
                .collect();

            v.into()
        }))
    }

    fn iter_raw<'b>(&'b self) -> Box<Iterator<Item = RawRow<Self::ColumnValue>> + 'b> {
        let row_iter = self.iter();
        Box::new(
            row_iter
                .zip(self.table.rows_set.iter())
                .map(|(row, raw_row)| RawRow { row, raw_row }),
        )
    }
}

impl DatabaseStorage for TempDb {
    type Info = TempDb;

    fn scan_table<'a>(&'a self, table: &'a Table) -> Box<Group<ColumnValue = Variant> + 'a> {
        Box::new(ScanGroup { table })
    }
}

impl TempDb {
    pub fn new() -> TempDb {
        TempDb { tables: Vec::new() }
    }

    /// Entry point for this database, executes specified sql and returns result.
    pub fn do_query(&mut self, sql: &str) -> ExecuteStatementResult {
        let statement = sqlsyntax::parse_statement(sql).map_err(ExecuteError::from)?;
        self.execute_statement(statement)
    }

    pub fn execute_statement(&mut self, stmt: ast::Statement) -> ExecuteStatementResult {
        match stmt {
            ast::Statement::Create(create_stmt) => match create_stmt {
                ast::CreateStatement::Table(s) => self.create_table(s),
            },
            ast::Statement::Drop(drop_stmt) => self.drop_table(drop_stmt),
            ast::Statement::Insert(insert_stmt) => self.insert_into(insert_stmt),
            ast::Statement::Select(select_stmt) => self.select(select_stmt),
            ast::Statement::Delete(delete_stmt) => self.delete(delete_stmt),
            ast::Statement::Truncate(truncate_stmt) => self.delete(truncate_stmt.into()),
            ast::Statement::Explain(explain_stmt) => self.explain(explain_stmt),
            ast::Statement::Update(update_stmt) => self.update(update_stmt),
        }
    }

    fn create_table(&mut self, stmt: ast::CreateTableStatement) -> ExecuteStatementResult {
        if stmt.table.database_name.is_some() {
            return Err(ExecuteError::new("Several databases is not supported."));
        }

        let table_name = Identifier::new(&stmt.table.table_name)
            .ok_or(ExecuteError::new("Invalid table name."))?;

        let columns = stmt
            .columns
            .into_iter()
            .enumerate()
            .map(|(i, column)| {
                let name = Identifier::new(&column.column_name)
                    .ok_or(ExecuteError::new("Column name is required."))?;
                let type_name = Identifier::new(&column.type_name)
                    .ok_or(ExecuteError::new("Type name is required."))?;
                let type_array_size = match column.type_array_size {
                    Some(Some(s)) => {
                        let v = self.parse_number_as_u64(s)?;
                        Some(Some(v))
                    },
                    Some(None) => Some(None),
                    None => None,
                };

                let dbtype = DbType::from_identifier(&type_name, type_array_size).ok_or(
                    ExecuteError::from_string(format!("{} is not a valid column type", type_name)),
                )?;

                let nullable = column
                    .constraints
                    .iter()
                    .any(|c| c.constraint == ast::CreateTableColumnConstraintType::Nullable);

                Ok(table::Column {
                    offset: i as u32,
                    name,
                    dbtype,
                    nullable,
                })
            })
            .collect::<Result<Vec<Column>, ExecuteError>>()?;

        self.add_table(Table {
            name: table_name,
            columns,
            next_rowid: 1,
            rows_set: BTreeSet::new(),
        })?;

        Ok(ExecuteStatementResponse::Created)
    }

    fn drop_table(&mut self, stmt: ast::DropTableStatement) -> ExecuteStatementResult {
        if stmt.table.database_name.is_some() {
            return Err(ExecuteError::new("Several databases is not supported."));
        }

        let table_name = Identifier::new(&stmt.table.table_name).ok_or(
            ExecuteError::from_string(format!("Bad table name: {}", &stmt.table.table_name)),
        )?;

        let table_idx = self
            .tables
            .iter()
            .enumerate()
            .find(|(_, table)| table.name == table_name)
            .map(|(idx, _)| idx);

        if let Some(idx) = table_idx {
            self.tables.remove(idx); // do remove table
            Ok(ExecuteStatementResponse::Dropped)
        } else {
            Err(ExecuteError::from_string(format!(
                "Table with name={} does not exist",
                table_name
            )))
        }
    }

    fn insert_into(&mut self, stmt: ast::InsertStatement) -> ExecuteStatementResult {
        trace!("inserting row: {:?}", stmt);

        let table_name = stmt.table.table_name;
        let column_types: Vec<(DbType, bool)>;
        let ast_index_to_column_index: Vec<u32>;

        {
            let table = self.get_table_mut(&table_name)?;

            column_types = table
                .get_columns()
                .iter()
                .map(|c| (c.dbtype, c.nullable))
                .collect();

            ast_index_to_column_index = match stmt.into_columns {
                // Column names listed; map specified columns
                Some(v) => v
                    .into_iter()
                    .map(|column_name| {
                        let ident = Identifier::new(&column_name)
                            .ok_or(ExecuteError::new("Invalid column name."))?;
                        match table.find_column_by_name(&ident) {
                            Some(column) => Ok(column.get_offset()),
                            None => Err(ExecuteError::from_string(format!(
                                "column {} is not in table",
                                column_name
                            ))),
                        }
                    })
                    .collect::<Result<Vec<u32>, ExecuteError>>()?,

                // No column names are listed; map all columns
                None => (0..table.get_column_count()).collect(),
            };

            trace!("ast_index_to_column_index: {:?}", ast_index_to_column_index);
        }

        match stmt.source {
            ast::InsertSource::Values(rows) => {
                let mut count = 0;

                for row in rows {
                    if ast_index_to_column_index.len() != row.len() {
                        return Err(ExecuteError::from_string(format!(
                            "INSERT value contains wrong amount of columns"
                        )));
                    }

                    let mut exprs: Vec<Option<ast::Expression>>;
                    exprs = (0..column_types.len()).map(|_| None).collect();

                    for (i, expr) in row.into_iter().enumerate() {
                        exprs[ast_index_to_column_index[i] as usize] = Some(expr);
                    }

                    // TODO: don't allow expressions that SELECT the same table that's being inserted into
                    let v: Vec<_> = {
                        column_types
                            .iter()
                            .zip(exprs.into_iter())
                            .map(|(&(dbtype, nullable), expr)| {
                                match expr {
                                    Some(expr) => {
                                        // TODO - allocate buffer outside of loop
                                        let mut buf = Vec::new();

                                        let execute = ExecuteQueryPlan::new(self);

                                        let sexpr = queryplan::compile_ast_expression(self, expr)
                                            .map_err(|e| ExecuteError::from(e))?;

                                        let value = execute.execute_expression(&sexpr)?;

                                        let is_null =
                                            variant_to_data(value, dbtype, nullable, &mut buf)?;
                                        Ok((buf.into_boxed_slice(), is_null))
                                    },
                                    None => {
                                        // use default value for column type
                                        let is_null = if nullable { Some(true) } else { None };
                                        Ok((
                                            dbtype.get_default().into_owned().into_boxed_slice(),
                                            is_null,
                                        ))
                                    },
                                }
                            })
                            .collect::<Result<Vec<_>, ExecuteError>>()
                    }?;

                    let mut table = self.get_table_mut(&table_name)?;
                    table
                        .insert_new_row(v.into_iter())
                        .map_err(|e| ExecuteError::from_string(e.to_string()))?;
                    count += 1;
                }

                Ok(ExecuteStatementResponse::Inserted(count))
            },
            ast::InsertSource::Select(_s) => Err(ExecuteError::new(
                "Inserts with 'select' statement is not supported.",
            )),
        }
    }

    fn select(&self, stmt: ast::SelectStatement) -> ExecuteStatementResult {
        let plan = QueryPlan::compile_select(self, stmt).map_err(ExecuteError::from)?;
        debug!("{}", plan);

        let mut rows = Vec::new();

        let execute = ExecuteQueryPlan::new(self);
        execute.execute_query_plan(
            &plan.expr,
            &mut |r| {
                rows.push(r.to_vec().into_boxed_slice());
                Ok(())
            },
            &mut |_| Ok(()),
        )?;

        let column_names: Vec<String> = plan
            .out_column_names
            .iter()
            .map(|ident| ident.to_string())
            .collect();

        Ok(ExecuteStatementResponse::Select {
            column_names: column_names.into_boxed_slice(),
            rows: Box::new(rows.into_iter()),
        })
    }

    fn delete(&mut self, stmt: ast::DeleteStatement) -> ExecuteStatementResult {
        trace!("deleting rows: {:?}", stmt);

        let stmt_clone = stmt.clone();

        match &stmt.table {
            TableOrSubquery::Table { table, .. } => {
                match stmt.where_expr {
                    None => {
                        let table = self.get_table_mut(&table.table_name)?;

                        // the same as Truncate, remove all rows from table
                        table
                            .truncate()
                            .map_err(ExecuteError::from)
                            .map(ExecuteStatementResponse::Deleted)
                    },
                    Some(_) => {
                        let mut selected_rows = HashSet::<Vec<u8>>::new();

                        {
                            let plan = QueryPlan::compile_delete(self, stmt_clone)
                                .map_err(ExecuteError::from)?;
                            debug!("delete plan: {}", plan);

                            let execute = ExecuteQueryPlan::new(self);
                            execute.execute_query_plan(
                                &plan.expr,
                                &mut |_| Ok(()),
                                &mut |row_as_bytes| {
                                    selected_rows.insert(row_as_bytes.clone());
                                    Ok(())
                                },
                            )?;
                        }

                        // do delete

                        let mut table = self.get_table_mut(&table.table_name)?;
                        let row_deleted = selected_rows.len();
                        for row in selected_rows {
                            table.delete_row(row.as_slice())?;
                        }

                        Ok(ExecuteStatementResponse::Deleted(row_deleted))
                    },
                }
            },
            _ => Err(ExecuteError::new(
                "Subqueries instead of a Table name isn't allowed.",
            )),
        }
    }

    fn explain(&self, stmt: ast::ExplainStatement) -> ExecuteStatementResult {
        use queryplan::QueryPlan;

        match stmt {
            ast::ExplainStatement::Select(select) => {
                let plan = QueryPlan::compile_select(self, select).map_err(ExecuteError::from)?;

                Ok(ExecuteStatementResponse::Explain(plan.to_string()))
            },
        }
    }

    fn update(&mut self, stmt: ast::UpdateStatement) -> ExecuteStatementResult {
        trace!("update statement {:?}", stmt);

        let stmt_clone = stmt.clone();
        let table_name;

        if let TableOrSubquery::Table { table, .. } = stmt.table {
            table_name = table.table_name
        } else {
            return Err(ExecuteError::new(
                "Expected name of updated table, got subquery.",
            ));
        }

        let column_types_in_table: Vec<(DbType, bool)> = {
            let table = self.get_table_mut(&table_name)?;

            table
                .get_columns()
                .iter()
                .map(|c| (c.dbtype, c.nullable))
                .collect()
        };

        let col_idx_to_ast_idx: HashMap<usize, ast::Expression> = {
            let table = self.get_table_mut(&table_name)?;

            stmt.update
                .into_iter()
                .map(
                    |UpdateField {
                         column_name,
                         new_value,
                     }| {
                        let ident = Identifier::new(&column_name)
                            .ok_or(ExecuteError::new("Invalid column name."))?;

                        let col_idx = match table.find_column_by_name(&ident) {
                            Some(column) => Ok(column.get_offset() as usize),
                            None => Err(ExecuteError::from_string(format!(
                                "column {} is not in table",
                                column_name
                            ))),
                        };

                        Ok((col_idx?, new_value))
                    },
                )
                .collect::<Result<HashMap<usize, ast::Expression>, ExecuteError>>()?
        };

        trace!("col_idx_to_ast_idx: {:?}", col_idx_to_ast_idx);

        let new_values_for_update = col_idx_to_ast_idx
            .into_iter()
            .map(|(col_offset, expr)| {
                let (dbtype, nullable) = column_types_in_table[col_offset]; // should never be failed
                let mut buf = Vec::new();

                let execute = ExecuteQueryPlan::new(self);

                let sexpr =
                    queryplan::compile_ast_expression(self, expr).map_err(ExecuteError::from)?;

                let value = execute.execute_expression(&sexpr)?;
                let is_null = variant_to_data(value, dbtype, nullable, &mut buf)?;

                Ok((col_offset, (buf.into_boxed_slice(), is_null)))
            })
            .collect::<Result<HashMap<usize, _>, ExecuteError>>()?;

        trace!("New values for update {:?}", new_values_for_update);

        let mut rows_selected_for_update = HashSet::<Vec<u8>>::new();

        {
            let plan = QueryPlan::compile_update(self, stmt_clone).map_err(ExecuteError::from)?;
            debug!("update plan: {}", plan);

            let execute = ExecuteQueryPlan::new(self);
            execute.execute_query_plan(&plan.expr, &mut |_| Ok(()), &mut |row_as_bytes| {
                // collect all selected rows for update
                rows_selected_for_update.insert(row_as_bytes.clone());
                Ok(())
            })?;
        }

        let table = self.get_table_mut(&table_name)?;
        let rows_updated = rows_selected_for_update.len();
        for old_row in rows_selected_for_update {
            table.update_row(old_row.as_slice(), &new_values_for_update)?;
        }

        Ok(Updated(rows_updated))
    }

    fn add_table(&mut self, table: Table) -> Result<(), ExecuteError> {
        if self.tables.iter().any(|t| t.name == table.name) {
            Err(ExecuteError::from_string(format!(
                "Table {} already exists",
                table.name
            )))
        } else {
            debug!("adding table: {:?}", table);
            self.tables.push(table);

            Ok(())
        }
    }

    fn get_table_mut(&mut self, table_name: &str) -> Result<&mut Table, ExecuteError> {
        let table_name = Identifier::new(table_name).ok_or(ExecuteError::from_string(format!(
            "Bad table name: {}",
            table_name
        )))?;

        self.tables
            .iter_mut()
            .find(|t| t.name == table_name)
            .ok_or(ExecuteError::from_string(format!(
                "Could not find table named {}",
                table_name
            )))
    }

    fn parse_number_as_u64(&self, number: String) -> Result<u64, ExecuteError> {
        number
            .parse()
            .map_err(|_| ExecuteError::from_string(format!("{} is not a valid number", number)))
    }
}

/// Converts current Variant to bytes.
fn variant_to_data(
    value: Variant,
    column_type: DbType,
    nullable: bool,
    buf: &mut Vec<u8>,
) -> Result<Option<bool>, ExecuteError> {
    match (value.is_null(), nullable) {
        (true, true) => Ok(Some(true)),
        (true, false) => Err(ExecuteError::from_string(format!(
            "cannot insert NULL into column that doesn't allow NULL"
        ))),
        (false, nullable) => {
            let bytes = value
                .to_bytes(column_type)
                .map_err(ExecuteError::from_string)?;
            buf.extend_from_slice(&bytes);

            Ok(if nullable { Some(false) } else { None })
        },
    }
}

#[cfg(test)]
mod test {
    use tempdb::ExecuteError;
    use tempdb::ExecuteStatementResponse;
    use tempdb::ExecuteStatementResult;
    use tempdb::TempDb;
    use types::Variant;

    fn create_table<'a>(db: &'a mut TempDb, t_name: &str) -> ExecuteStatementResult<'a> {
        db.do_query(&format!(
            "create table {}(id int, name text, age int);",
            t_name
        ))
    }

    fn fill_table<'a>(db: &'a mut TempDb, t_name: &str) -> ExecuteStatementResult<'a> {
        db.do_query(&format!(
            "insert into {} values(1, 'Isaac Asimov', 50);",
            t_name
        ))?;
        db.do_query(&format!(
            "insert into {} values(2, 'Stanislaw Lem', 40)",
            t_name
        ))?;
        db.do_query(&format!(
            "insert into {} values(3, 'Liu Cixin', 30)",
            t_name
        ))
    }

    fn row_in_table(db: &mut TempDb, t_name: &str) -> Result<usize, ExecuteError> {
        row_in_table_where(db, t_name, "")
    }

    fn row_in_table_where(
        db: &mut TempDb,
        t_name: &str,
        where_condition: &str,
    ) -> Result<usize, ExecuteError> {
        let res = db.do_query(&format!(
            "select count(*) from {} {};",
            t_name, where_condition
        ))?;
        let result = match res {
            ExecuteStatementResponse::Select {
                column_names: _,
                rows,
            } => {
                let rows = rows.collect::<Vec<Box<[Variant]>>>();
                if rows.len() == 0 {
                    Ok(0)
                } else {
                    let first_row = rows.get(0).unwrap();
                    let first_rec = &first_row[0];
                    let result = match first_rec {
                        Variant::UnsignedInteger(int) => *int as usize,
                        _ => panic!("Can't get count(*)"),
                    };
                    Ok(result)
                }
            },
            _ => Err(ExecuteError::new("Can't get count(*)")),
        };
        result
    }

    fn row_to_string(row: Box<[Variant]>) -> String {
        row.iter()
            .map(|elem| elem.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn rows_to_strings<'a>(rows: Box<Iterator<Item = Box<[Variant]>> + 'a>) -> Vec<String> {
        rows.into_iter().map(row_to_string).collect()
    }

    #[test]
    fn select_test() {
        let db = &mut TempDb::new();

        create_table(db, "Users").unwrap();
        fill_table(db, "Users").unwrap();

        match db.do_query("select * from Users;").unwrap() {
            ExecuteStatementResponse::Select { column_names, rows } => {
                assert_eq!(column_names.to_vec(), vec!["id", "name", "age"]);
                assert_eq!(
                    rows_to_strings(rows),
                    vec![
                        "1, Isaac Asimov, 50",
                        "2, Stanislaw Lem, 40",
                        "3, Liu Cixin, 30"
                    ]
                );
            },
            _ => panic!("Expected Select result"),
        };

        match db.do_query("select min(id) as min, name from Users where age >= 30 and name <> 'Roger Zelazny'").unwrap() {
            ExecuteStatementResponse::Select { column_names, rows } => {
                assert_eq!(column_names.to_vec(), vec!["min", "name"]);
                assert_eq!(
                    rows_to_strings(rows),
                    vec!["1, Isaac Asimov"]
                );
            },
            _ => panic!("Expected Select result"),
        };

        match db
            .do_query("select avg(age) as avg, name from Users group by age having id > 2")
            .unwrap()
        {
            ExecuteStatementResponse::Select { column_names, rows } => {
                assert_eq!(column_names.to_vec(), vec!["avg", "name"]);
                assert_eq!(rows_to_strings(rows), vec!["30, Liu Cixin"]);
            },
            _ => panic!("Expected Select result"),
        };

        match db
            .do_query(
                "select min(age) as min, max(age) as max, count(age) as count, sum(age) as \
                 sum, avg(age) as avg FROM Users",
            )
            .unwrap()
        {
            ExecuteStatementResponse::Select { column_names, rows } => {
                assert_eq!(
                    column_names.to_vec(),
                    vec!["min", "max", "count", "sum", "avg"]
                );
                assert_eq!(rows_to_strings(rows), vec!["30, 50, 3, 120, 40"]);
            },
            _ => panic!("Expected Select result"),
        };

        // todo add more test cases, including join, sub queries and so on
    }

    #[test]
    fn delete_test() {
        let db = &mut TempDb::new();
        create_table(db, "Users").unwrap();
        fill_table(db, "Users").unwrap();

        assert_eq!(row_in_table(db, "Users").unwrap(), 3);

        match db.do_query("delete from Users;").unwrap() {
            ExecuteStatementResponse::Deleted(number_of_rows) => assert_eq!(number_of_rows, 3),
            _ => panic!("Expected Deleted result"),
        };

        assert_eq!(row_in_table(db, "Users").unwrap(), 0);

        fill_table(db, "Users").unwrap();
        assert_eq!(row_in_table(db, "Users").unwrap(), 3);

        match db.do_query("delete * from Users;").unwrap() {
            ExecuteStatementResponse::Deleted(number_of_rows) => assert_eq!(number_of_rows, 3),
            _ => panic!("Expected Deleted result"),
        };

        assert_eq!(row_in_table(db, "Users").unwrap(), 0);

        fill_table(db, "Users").unwrap();
        assert_eq!(row_in_table(db, "Users").unwrap(), 3);

        match db.do_query("delete from Users where id > 1;").unwrap() {
            ExecuteStatementResponse::Deleted(number_of_rows) => assert_eq!(number_of_rows, 2),
            _ => panic!("Expected Deleted result"),
        };

        println!("row in table {:?}", row_in_table(db, "Users").unwrap());
        assert_eq!(row_in_table(db, "Users").unwrap(), 1);

        fill_table(db, "Users").unwrap();
        assert_eq!(row_in_table(db, "Users").unwrap(), 4);

        match db
            .do_query(
                "delete from Users as u1 where u1.id = 1 or u1.age = \
                 (select u2.age from Users as u2 where u2.id = 2);",
            )
            .unwrap()
        {
            ExecuteStatementResponse::Deleted(number_of_rows) => assert_eq!(number_of_rows, 3),
            _ => panic!("Expected Deleted result"),
        };

        println!("row in table {:?}", row_in_table(db, "Users").unwrap());
        assert_eq!(row_in_table(db, "Users").unwrap(), 1);
    }

    #[test]
    fn truncate_test() {
        let db = &mut TempDb::new();
        create_table(db, "Users").unwrap();
        fill_table(db, "Users").unwrap();

        assert_eq!(row_in_table(db, "Users").unwrap(), 3);

        match db.do_query("truncate table Users;").unwrap() {
            ExecuteStatementResponse::Deleted(number_of_rows) => assert_eq!(number_of_rows, 3),
            _ => panic!("Expected Deleted result"),
        };

        assert_eq!(row_in_table(db, "Users").unwrap(), 0);
    }

    #[test]
    fn drop_test() {
        let db = &mut TempDb::new();
        create_table(db, "Users").unwrap();
        fill_table(db, "Users").unwrap();

        assert_eq!(row_in_table(db, "Users").unwrap(), 3);

        match db.do_query("drop table Users;").unwrap() {
            ExecuteStatementResponse::Dropped => true,
            _ => panic!("Expected Drop result"),
        };

        match db.do_query("select * from Users") {
            Ok(_) => panic!("Expected error result for this statement"),
            Err(ExecuteError { message }) => {
                assert_eq!(message.clone(), "table does not exist: users")
            },
        };
    }

    #[test]
    fn update_test() {
        let db = &mut TempDb::new();
        create_table(db, "Users").unwrap();
        fill_table(db, "Users").unwrap();

        assert_eq!(row_in_table(db, "Users").unwrap(), 3);

        match db.do_query("update Users set age = 0").unwrap() {
            ExecuteStatementResponse::Updated(number_of_rows) => assert_eq!(number_of_rows, 3),
            _ => panic!("Expected Update result"),
        };
        assert_eq!(row_in_table_where(db, "Users", "where age = 0").unwrap(), 3);

        match db
            .do_query(
                "update Users as u set u.name = 'unknown', u.age = -1 \
                 where u.name = (select u3.name from Users u3 where u3.id = 1);",
            )
            .unwrap()
        {
            ExecuteStatementResponse::Updated(number_of_rows) => assert_eq!(number_of_rows, 1),
            _ => panic!("Expected Update result"),
        };
        assert_eq!(
            row_in_table_where(db, "Users", "where name = 'unknown' and age = -1").unwrap(),
            1
        );
    }
}
