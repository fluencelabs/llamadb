use databaseinfo::{DatabaseInfo, TableInfo};

use std::fmt;
use std::fmt::Display;

#[derive(Clone)]
pub struct IfChain<'a, DB: DatabaseInfo>
where
    <DB as DatabaseInfo>::Table: 'a,
{
    pub predicate: SExpression<'a, DB>,
    pub yield_fn: SExpression<'a, DB>,
}

#[derive(Clone)]
pub enum SExpression<'a, DB: DatabaseInfo>
where
    <DB as DatabaseInfo>::Table: 'a,
{
    Scan {
        table: &'a <DB as DatabaseInfo>::Table,
        source_id: u32,
        yield_fn: Box<SExpression<'a, DB>>,
    },
    LeftJoin {
        source_id: u32,
        yield_in_fn: Box<SExpression<'a, DB>>,
        predicate: Box<SExpression<'a, DB>>,
        yield_out_fn: Box<SExpression<'a, DB>>,
        right_rows_if_none: Vec<<DB as DatabaseInfo>::ColumnValue>,
    },
    Map {
        source_id: u32,
        yield_in_fn: Box<SExpression<'a, DB>>,
        yield_out_fn: Box<SExpression<'a, DB>>,
    },
    TempGroupBy {
        source_id: u32,
        yield_in_fn: Box<SExpression<'a, DB>>,
        group_by_values: Vec<SExpression<'a, DB>>,
        yield_out_fn: Box<SExpression<'a, DB>>,
    },
    Yield {
        fields: Vec<SExpression<'a, DB>>,
    },
    ColumnField {
        source_id: u32,
        column_offset: u32,
    },
    If {
        chains: Vec<IfChain<'a, DB>>,
        /// Run if all predicates were false.
        else_: Option<Box<SExpression<'a, DB>>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<SExpression<'a, DB>>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<SExpression<'a, DB>>,
        rhs: Box<SExpression<'a, DB>>,
    },
    AggregateOp {
        op: AggregateOp,
        source_id: u32,
        value: Box<SExpression<'a, DB>>,
    },
    CountAll {
        source_id: u32,
    },
    Value(<DB as DatabaseInfo>::ColumnValue),
}

impl<'a, DB: DatabaseInfo> fmt::Display for SExpression<'a, DB>
where
    <DB as DatabaseInfo>::Table: 'a,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.format(f, 0)
    }
}

impl<'a, DB: DatabaseInfo> SExpression<'a, DB>
where
    <DB as DatabaseInfo>::Table: 'a,
{
    fn format(&self, f: &mut fmt::Formatter, indent: usize) -> Result<(), fmt::Error> {
        macro_rules! write_indent {
            ($i:expr) => {
                for _ in 0..$i {
                    try!(write!(f, "  "));
                }
            };
        }

        write_indent!(indent);

        match self {
            &SExpression::Scan {
                table,
                source_id,
                ref yield_fn,
            } => {
                writeln!(f, "(scan `{}` :source-id {}", table.get_name(), source_id)?;
                yield_fn.format(f, indent + 1)?;
                write!(f, ")")
            },
            &SExpression::LeftJoin {
                source_id,
                ref yield_in_fn,
                ref predicate,
                ref yield_out_fn,
                ref right_rows_if_none,
            } => {
                writeln!(f, "(left-join :source-id {}", source_id)?;
                yield_in_fn.format(f, indent + 1)?;
                writeln!(f, "")?;
                predicate.format(f, indent + 1)?;
                writeln!(f, "")?;
                yield_out_fn.format(f, indent + 1)?;
                writeln!(f, "")?;
                write_indent!(indent + 1);
                write!(f, "(right-rows-if-none ")?;
                for value in right_rows_if_none {
                    write!(f, "{} ", value)?;
                }
                write!(f, "))")
            },
            &SExpression::Map {
                source_id,
                ref yield_in_fn,
                ref yield_out_fn,
            } => {
                writeln!(f, "(map :source-id {}", source_id)?;
                yield_in_fn.format(f, indent + 1)?;
                writeln!(f, "")?;
                yield_out_fn.format(f, indent + 1)?;
                write!(f, ")")
            },
            &SExpression::TempGroupBy {
                source_id,
                ref yield_in_fn,
                ref group_by_values,
                ref yield_out_fn,
            } => {
                writeln!(f, "(temp-group-by :source-id {}", source_id)?;
                yield_in_fn.format(f, indent + 1)?;
                writeln!(f, "")?;
                write_indent!(indent + 1);
                write!(f, "(group-by-values")?;
                for group_by_value in group_by_values {
                    writeln!(f, "")?;
                    group_by_value.format(f, indent + 2)?;
                }
                writeln!(f, ")")?;
                yield_out_fn.format(f, indent + 1)?;
                write!(f, ")")
            },
            &SExpression::Yield { ref fields } => {
                write!(f, "(yield")?;
                for field in fields {
                    writeln!(f, "")?;
                    field.format(f, indent + 1)?;
                }
                write!(f, ")")
            },
            &SExpression::ColumnField {
                source_id,
                column_offset,
            } => write!(
                f,
                "(column-field :source-id {} :column-offset {})",
                source_id, column_offset
            ),
            &SExpression::If {
                ref chains,
                ref else_,
            } => {
                write!(f, "(if ")?;
                for chain in chains {
                    writeln!(f, "")?;
                    chain.predicate.format(f, indent + 1)?;
                    writeln!(f, "")?;
                    chain.yield_fn.format(f, indent + 1)?;
                }
                if let Some(e) = else_.as_ref() {
                    writeln!(f, "")?;
                    e.format(f, indent + 1)?;
                }
                write!(f, ")")
            },
            &SExpression::BinaryOp {
                ref op,
                ref lhs,
                ref rhs,
            } => {
                writeln!(f, "({} ", op.sigil())?;
                lhs.format(f, indent + 1)?;
                writeln!(f, "")?;
                rhs.format(f, indent + 1)?;
                write!(f, ")")
            },
            &SExpression::UnaryOp { ref op, ref expr } => {
                writeln!(f, "({} ", op.name())?;
                expr.format(f, indent + 1)?;
                write!(f, ")")
            },
            &SExpression::AggregateOp {
                ref op,
                source_id,
                ref value,
            } => {
                writeln!(f, "({} :source-id {} ", op.name(), source_id)?;
                value.format(f, indent + 1)?;
                write!(f, ")")
            },
            &SExpression::CountAll { source_id } => {
                write!(f, "(count-all :source-id {})", source_id)
            },
            &SExpression::Value(ref v) => write!(f, "{}", v),
        }
    }
}

#[derive(Copy, Clone)]
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

impl BinaryOp {
    fn sigil(&self) -> &'static str {
        use self::BinaryOp::*;

        match self {
            &Equal => "=",
            &NotEqual => "<>",
            &LessThan => "<",
            &LessThanOrEqual => "<=",
            &GreaterThan => ">",
            &GreaterThanOrEqual => ">=",
            &And => "and",
            &Or => "or",
            &Add => "+",
            &Subtract => "-",
            &Multiply => "*",
            &Divide => "/",
            &BitAnd => "&",
            &BitOr => "|",
            &Concatenate => "concat",
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Binary operation {}", self.sigil())
    }
}

#[derive(Copy, Clone)]
pub enum UnaryOp {
    Negate,
}

impl UnaryOp {
    fn name(&self) -> &'static str {
        use self::UnaryOp::*;

        match self {
            &Negate => "negate",
        }
    }
}

#[derive(Copy, Clone)]
pub enum AggregateOp {
    Count,
    Avg,
    Sum,
    Min,
    Max,
}

impl AggregateOp {
    fn name(&self) -> &'static str {
        use self::AggregateOp::*;

        match self {
            &Count => "count",
            &Avg => "avg",
            &Sum => "sum",
            &Min => "min",
            &Max => "max",
        }
    }
}
