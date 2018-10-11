use sqlsyntax::ast::Statement;
use sqlsyntax::lexer::LexerError;
use sqlsyntax::parser::RuleError;
use std::error::Error;
use std::fmt;
use std::fmt::Display;

/// As of writing, there aren't any good or stable LALR(1) parser generators for Rust.
/// As a consequence, the lexer and parser are both written by hand.
pub mod ast;
pub mod lexer;
pub mod parser;

#[derive(PartialEq, Debug)]
pub struct ParseError {
    message: String,
}

impl ParseError {
    pub fn new(message: &'static str) -> Self {
        ParseError {
            message: message.to_string(),
        }
    }
}

impl From<RuleError> for ParseError {
    fn from(err: RuleError) -> Self {
        ParseError {
            message: err.to_string(),
        }
    }
}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError {
            message: err.to_string(),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Parse error: {}", self.message)
    }
}

impl Error for ParseError {}

/// Parses single sql statement and returns AST.
pub fn parse_statement(query: &str) -> Result<Statement, ParseError> {
    let tokens = lexer::parse(query)?;
    parser::parse_statement(&tokens).map_err(Into::into)
}

/// Parses a series of sql statements separated by semicolons and returns
/// sequence of corresponded AST representations.
pub fn parse_statements(query: &str) -> Result<Vec<ast::Statement>, ParseError> {
    let tokens = lexer::parse(query)?;
    parser::parse_statements(&tokens).map_err(Into::into)
}

#[cfg(test)]
mod test {
    use super::parse_statement as parse;
    use sqlsyntax::ast;
    use sqlsyntax::ast::BinaryOp::Add;
    use sqlsyntax::ast::BinaryOp::Concatenate;
    use sqlsyntax::ast::BinaryOp::Divide;
    use sqlsyntax::ast::BinaryOp::Equal;
    use sqlsyntax::ast::BinaryOp::GreaterThan;
    use sqlsyntax::ast::CreateStatement;
    use sqlsyntax::ast::CreateTableColumn;
    use sqlsyntax::ast::CreateTableColumnConstraint;
    use sqlsyntax::ast::CreateTableColumnConstraintType::Nullable;
    use sqlsyntax::ast::CreateTableColumnConstraintType::PrimaryKey;
    use sqlsyntax::ast::CreateTableColumnConstraintType::Unique;
    use sqlsyntax::ast::CreateTableStatement;
    use sqlsyntax::ast::ExplainStatement::Select;
    use sqlsyntax::ast::Expression::BinaryOp;
    use sqlsyntax::ast::Expression::FunctionCall;
    use sqlsyntax::ast::Expression::FunctionCallAggregateAll;
    use sqlsyntax::ast::Expression::Ident;
    use sqlsyntax::ast::Expression::IdentMember;
    use sqlsyntax::ast::Expression::Number;
    use sqlsyntax::ast::Expression::StringLiteral;
    use sqlsyntax::ast::From::Cross;
    use sqlsyntax::ast::From::Join;
    use sqlsyntax::ast::InsertSource;
    use sqlsyntax::ast::InsertStatement;
    use sqlsyntax::ast::JoinOperator::Inner;
    use sqlsyntax::ast::Order::Ascending;
    use sqlsyntax::ast::Order::Descending;
    use sqlsyntax::ast::OrderingTerm;
    use sqlsyntax::ast::SelectColumn::AllColumns;
    use sqlsyntax::ast::SelectColumn::Expr;
    use sqlsyntax::ast::SelectStatement;
    use sqlsyntax::ast::Statement;
    use sqlsyntax::ast::Table;
    use sqlsyntax::ast::TableOrSubquery;
    use sqlsyntax::ast::TableOrSubquery::Subquery;
    use sqlsyntax::ParseError;
    use std::option::Option::Some;

    #[test]
    fn create_table_parsing_test() {
        let create_table = parse(
            "CREATE TABLE test (
            foo     INT CONSTRAINT pk PRIMARY KEY,
            bar     VARCHAR(256),
            data    BYTE[32] NULL UNIQUE
        );",
        );

        match create_table.unwrap() {
            Statement::Create(CreateStatement::Table(CreateTableStatement { table, columns })) => {
                assert_eq!(
                    table,
                    Table {
                        database_name: None,
                        table_name: "test".to_string()
                    }
                );
                assert_eq!(
                    columns,
                    vec![
                        CreateTableColumn {
                            column_name: "foo".to_string(),
                            type_name: "INT".to_string(),
                            type_size: None,
                            type_array_size: None,
                            constraints: vec![CreateTableColumnConstraint {
                                name: Some("pk".to_string()),
                                constraint: PrimaryKey
                            }]
                        },
                        CreateTableColumn {
                            column_name: "bar".to_string(),
                            type_name: "VARCHAR".to_string(),
                            type_size: Some("256".to_string()),
                            type_array_size: None,
                            constraints: Vec::new()
                        },
                        CreateTableColumn {
                            column_name: "data".to_string(),
                            type_name: "BYTE".to_string(),
                            type_size: None,
                            type_array_size: Some(Some("32".to_string())),
                            constraints: vec![
                                CreateTableColumnConstraint {
                                    name: None,
                                    constraint: Nullable
                                },
                                CreateTableColumnConstraint {
                                    name: None,
                                    constraint: Unique
                                }
                            ]
                        }
                    ]
                );
            },
            st => panic!("Expected create statement but actually={:?}", st),
        }
    }

    #[test]
    fn insert_parsing_test() {
        let insert_sql1 = parse("INSERT INTO table1 VALUES (1, 2), (3, 4), (5, 6);");
        match insert_sql1.unwrap() {
            Statement::Insert(InsertStatement {
                table,
                into_columns,
                source,
            }) => {
                assert_eq!(
                    table,
                    Table {
                        database_name: None,
                        table_name: "table1".to_string(),
                    }
                );
                assert_eq!(into_columns, None);
                assert_eq!(
                    source,
                    InsertSource::Values(vec![
                        vec![Number("1".to_string()), Number("2".to_string())],
                        vec![Number("3".to_string()), Number("4".to_string())],
                        vec![Number("5".to_string()), Number("6".to_string())]
                    ])
                );
            },
            st => panic!("Expected insert but actually={:?}", st),
        }
        let insert_sql2 = parse("INSERT INTO table1 (a, b) VALUES ('foo' || 'bar', 2);");
        match insert_sql2.unwrap() {
            Statement::Insert(InsertStatement {
                table,
                into_columns,
                source,
            }) => {
                assert_eq!(
                    table,
                    Table {
                        database_name: None,
                        table_name: "table1".to_string(),
                    }
                );
                assert_eq!(into_columns, Some(vec!["a".to_string(), "b".to_string()]));
                assert_eq!(
                    source,
                    InsertSource::Values(vec![vec![
                        BinaryOp {
                            lhs: Box::new(StringLiteral("foo".to_string())),
                            rhs: Box::new(StringLiteral("bar".to_string())),
                            op: Concatenate
                        },
                        Number("2".to_string())
                    ]])
                );
            },
            st => panic!("Expected insert but actually={:?}", st),
        }
        let insert_sql3 = parse("INSERT INTO table1 SELECT * FROM foo;");
        match insert_sql3.unwrap() {
            Statement::Insert(InsertStatement {
                table,
                into_columns,
                source,
            }) => {
                assert_eq!(
                    table,
                    Table {
                        database_name: None,
                        table_name: "table1".to_string(),
                    }
                );
                assert_eq!(into_columns, None);
                assert_eq!(
                    source,
                    InsertSource::Select(Box::new(SelectStatement {
                        result_columns: vec![AllColumns],
                        from: Cross(vec![TableOrSubquery::Table {
                            table: Table {
                                database_name: None,
                                table_name: "foo".to_string()
                            },
                            alias: None
                        }]),
                        where_expr: None,
                        group_by: vec![],
                        having: None,
                        order_by: vec![]
                    }))
                );
            },
            st => panic!("Expected insert but actually={:?}", st),
        }
    }

    #[test]
    fn select_parsing_test() {
        let select_sql1 = parse("SELECT *, (name + 4), count(*) AS amount FROM (SELECT * FROM foo) subq, table1 GROUP BY name HAVING count(*) > 5;");
        match select_sql1.unwrap() {
            Statement::Select(SelectStatement {
                result_columns,
                from,
                where_expr,
                group_by,
                having,
                order_by,
            }) => {
                assert_eq!(
                    result_columns,
                    vec![
                        AllColumns,
                        Expr {
                            expr: BinaryOp {
                                lhs: Box::new(Ident("name".to_string())),
                                rhs: Box::new(Number("4".to_string())),
                                op: Add
                            },
                            alias: None
                        },
                        Expr {
                            expr: FunctionCallAggregateAll {
                                name: "count".to_string()
                            },
                            alias: Some("amount".to_string())
                        }
                    ]
                );
                assert_eq!(
                    from,
                    Cross(vec![
                        Subquery {
                            subquery: Box::new(SelectStatement {
                                result_columns: vec![AllColumns],
                                from: Cross(vec![TableOrSubquery::Table {
                                    table: Table {
                                        database_name: None,
                                        table_name: "foo".to_string()
                                    },
                                    alias: None
                                }]),
                                where_expr: None,
                                group_by: vec![],
                                having: None,
                                order_by: vec![]
                            }),
                            alias: "subq".to_string()
                        },
                        TableOrSubquery::Table {
                            table: Table {
                                database_name: None,
                                table_name: "table1".to_string()
                            },
                            alias: None
                        }
                    ])
                );
                assert_eq!(where_expr, None);
                assert_eq!(group_by, vec![Ident("name".to_string())]);
                assert_eq!(
                    having,
                    Some(BinaryOp {
                        lhs: Box::new(FunctionCallAggregateAll {
                            name: "count".to_string()
                        }),
                        rhs: Box::new(Number("5".to_string())),
                        op: GreaterThan
                    })
                );
                assert_eq!(order_by, Vec::new());
            },
            st => panic!("Expected select statement but actually={:?}", st),
        }

        let select_sql2 =
            parse("SELECT * FROM foo INNER JOIN bar ON foo.id = bar.fooId ORDER BY a DESC, b;");
        match select_sql2.unwrap() {
            Statement::Select(SelectStatement {
                result_columns,
                from,
                where_expr,
                group_by,
                having,
                order_by,
            }) => {
                assert_eq!(result_columns, vec![AllColumns]);
                assert_eq!(
                    from,
                    Join {
                        table: TableOrSubquery::Table {
                            table: Table {
                                database_name: None,
                                table_name: "foo".to_string()
                            },
                            alias: None
                        },
                        joins: vec![ast::Join {
                            operator: Inner,
                            table: TableOrSubquery::Table {
                                table: Table {
                                    database_name: None,
                                    table_name: "bar".to_string()
                                },
                                alias: None
                            },
                            on: BinaryOp {
                                lhs: Box::new(IdentMember("foo".to_string(), "id".to_string())),
                                rhs: Box::new(IdentMember("bar".to_string(), "fooId".to_string())),
                                op: Equal
                            }
                        }]
                    }
                );
                assert_eq!(where_expr, None);
                assert_eq!(group_by, Vec::new());
                assert_eq!(having, None);
                assert_eq!(
                    order_by,
                    vec![
                        OrderingTerm {
                            expr: Ident("a".to_string()),
                            order: Descending
                        },
                        OrderingTerm {
                            expr: Ident("b".to_string()),
                            order: Ascending
                        }
                    ]
                );
            },
            st => panic!("Expected select statement but actually={:?}", st),
        }
        let select_sql3 = parse("SELECT avg(milliseconds) / 1000 seconds FROM track;");
        match select_sql3.unwrap() {
            Statement::Select(SelectStatement {
                result_columns,
                from,
                where_expr,
                group_by,
                having,
                order_by,
            }) => {
                assert_eq!(
                    result_columns,
                    vec![Expr {
                        expr: BinaryOp {
                            lhs: Box::new(FunctionCall {
                                name: "avg".to_string(),
                                arguments: vec![Ident("milliseconds".to_string())]
                            }),
                            rhs: Box::new(Number("1000".to_string())),
                            op: Divide
                        },
                        alias: Some("seconds".to_string())
                    }]
                );
                assert_eq!(
                    from,
                    Cross(vec![TableOrSubquery::Table {
                        table: Table {
                            database_name: None,
                            table_name: "track".to_string()
                        },
                        alias: None
                    }])
                );
                assert_eq!(where_expr, None);
                assert_eq!(group_by, Vec::new());
                assert_eq!(having, None);
                assert_eq!(order_by, Vec::new());
            },
            st => panic!("Expected select statement but actually={:?}", st),
        }
    }

    #[test]
    fn explain_parsing_test() {
        let explain_sql = parse(
            "EXPLAIN select avg(milliseconds) / 1000 seconds FROM track where name = 'test';",
        );
        match explain_sql.unwrap() {
            Statement::Explain(Select(SelectStatement {
                result_columns,
                from,
                where_expr,
                group_by,
                having,
                order_by,
            })) => {
                assert_eq!(
                    result_columns,
                    vec![Expr {
                        expr: BinaryOp {
                            lhs: Box::new(FunctionCall {
                                name: "avg".to_string(),
                                arguments: vec![Ident("milliseconds".to_string())]
                            }),
                            rhs: Box::new(Number("1000".to_string())),
                            op: Divide
                        },
                        alias: Some("seconds".to_string())
                    }]
                );
                assert_eq!(
                    from,
                    Cross(vec![TableOrSubquery::Table {
                        table: Table {
                            database_name: None,
                            table_name: "track".to_string()
                        },
                        alias: None
                    }])
                );
                assert_eq!(
                    where_expr,
                    Some(BinaryOp {
                        lhs: Box::new(Ident("name".to_string())),
                        rhs: Box::new(StringLiteral("test".to_string())),
                        op: Equal
                    })
                );
                assert_eq!(group_by, Vec::new());
                assert_eq!(having, None);
                assert_eq!(order_by, Vec::new());
            },
            st => panic!("Expected explain statement but actually={:?}", st),
        }
    }

    #[test]
    fn parsing_errors_test() {
        match parse("") {
            Err(err) => assert_eq!(
                err,
                ParseError::new(
                    "Expected SELECT, INSERT, CREATE, or EXPLAIN statement; got no more tokens"
                )
            ),
            st => panic!("Expected error but actually={:?}", st),
        }
        match parse("123") {
            Err(err) => assert_eq!(
                err,
                ParseError::new(
                    "Expected SELECT, INSERT, CREATE, or EXPLAIN statement; got Number(\"123\")"
                )
            ),
            st => panic!("Expected error but actually={:?}", st),
        }
        match parse("select select from USERS") {
            Err(err) => assert_eq!(
                err,
                ParseError::new("Expected * or expression for SELECT column; got Select")
            ),
            st => panic!("Expected error but actually={:?}", st),
        }
    }

    #[test]
    fn lexer_error_test() {
        match parse("†††") {
            Err(err) => assert_eq!(err, ParseError::new("Lexer error: Unknown character †")),
            st => panic!("Expected error but actually={:?}", st),
        }
    }
}
