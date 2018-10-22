#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    Add,
    Subtract,
    Multiply,
    Divide,
    BitAnd,
    BitOr,
    Concatenate,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(String),
    IdentMember(String, String),
    StringLiteral(String),
    Number(String),
    Null,
    /// name(argument1, argument2, argument3...)
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
    /// name(*)
    FunctionCallAggregateAll {
        name: String,
    },
    UnaryOp {
        expr: Box<Expression>,
        op: UnaryOp,
    },
    /// lhs op rhs
    BinaryOp {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinaryOp,
    },
    Subquery(Box<SelectStatement>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Table {
    pub database_name: Option<String>,
    pub table_name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TableOrSubquery {
    Subquery {
        subquery: Box<SelectStatement>,
        alias: String,
    },
    Table {
        table: Table,
        alias: Option<String>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum SelectColumn {
    AllColumns,
    Expr {
        expr: Expression,
        alias: Option<String>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelectStatement {
    pub result_columns: Vec<SelectColumn>,
    pub from: From,
    pub where_expr: Option<Expression>,
    pub group_by: Vec<Expression>,
    pub having: Option<Expression>,
    pub order_by: Vec<OrderingTerm>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum From {
    Cross(Vec<TableOrSubquery>),
    Join {
        table: TableOrSubquery,
        joins: Vec<Join>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum JoinOperator {
    Left,
    Inner,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Join {
    pub operator: JoinOperator,
    pub table: TableOrSubquery,
    pub on: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Order {
    Ascending,
    Descending,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OrderingTerm {
    pub expr: Expression,
    pub order: Order,
}

#[derive(Debug)]
pub struct InsertStatement {
    /// Table for inserting
    pub table: Table,

    /// Column names for filling when inserting
    pub into_columns: Option<Vec<String>>,

    /// New records for inserting.
    pub source: InsertSource,
}

#[derive(Debug, PartialEq)]
pub enum InsertSource {
    Values(Vec<Vec<Expression>>),
    Select(Box<SelectStatement>),
}

#[derive(Debug, PartialEq)]
pub struct CreateTableColumnConstraint {
    pub name: Option<String>,
    pub constraint: CreateTableColumnConstraintType,
}

#[derive(Debug, PartialEq)]
pub enum CreateTableColumnConstraintType {
    PrimaryKey,
    Unique,
    Nullable,
    ForeignKey {
        table: Table,
        columns: Option<Vec<String>>,
    },
}

#[derive(Debug, PartialEq)]
pub struct CreateTableColumn {
    pub column_name: String,
    pub type_name: String,
    pub type_size: Option<String>,
    /// * None if no array
    /// * Some(None) if dynamic array: type[]
    /// * Some(Some(_)) if fixed array: type[SIZE]
    pub type_array_size: Option<Option<String>>,
    pub constraints: Vec<CreateTableColumnConstraint>,
}

#[derive(Debug)]
pub struct CreateTableStatement {
    pub table: Table,
    pub columns: Vec<CreateTableColumn>,
}

#[derive(Debug)]
pub enum CreateStatement {
    Table(CreateTableStatement),
}

#[derive(Debug, Clone)]
pub struct DeleteStatement {
    /// Table for deleting row.
    pub table: TableOrSubquery,
    /// 'Where' condition
    pub where_expr: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct TruncateStatement {
    /// Table for clearing.
    pub table: Table,
}

#[derive(Debug)]
pub enum ExplainStatement {
    Select(SelectStatement),
}

#[derive(Debug, Clone)]
pub struct UpdateStatement {
    /// Table for update.
    pub table: TableOrSubquery,

    /// Column names and value for updating.
    /// Representation for `SET col1 = val1, col2 = val2, ...` in update statement.
    pub update: Vec<UpdateField>, // tood change to Vec<UpdateField>

    /// 'Where' conditions.
    pub where_expr: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UpdateField {
    /// Column name to assignment.
    pub column_name: String,

    /// New record for updating.
    pub new_value: Expression,
}

#[derive(Debug)]
pub enum Statement {
    Select(SelectStatement),
    Insert(InsertStatement),
    Create(CreateStatement),
    Delete(DeleteStatement),
    Truncate(TruncateStatement),
    Explain(ExplainStatement),
    Update(UpdateStatement),
}
