use sqlsyntax::parser::RuleError;
use sqlsyntax::ast::Statement;

/// As of writing, there aren't any good or stable LALR(1) parser generators for Rust.
/// As a consequence, the lexer and parser are both written by hand.

pub mod ast;
pub mod lexer;
pub mod parser;

pub fn parse_statement(query: &str) -> Result<Statement, RuleError> {
    let tokens = lexer::parse(query);
    parser::parse_statement(&tokens)
}

pub fn parse_statements(query: &str) -> Vec<ast::Statement> {
    let tokens = lexer::parse(query);
    parser::parse_statements(&tokens).unwrap()
}

#[cfg(test)]
mod test {
    use super::parse_statement as parse;

    // todo improve tests
    #[test]
    fn test_sql_parser() {
        parse("SELECT *, (name + 4), count(*) AS amount FROM (SELECT * FROM foo) subq, table1 GROUP BY name HAVING count(*) > 5;");
        parse("SELECT * FROM foo INNER JOIN bar ON foo.id = bar.fooId ORDER BY a DESC, b;");
        parse("SELECT avg(milliseconds) / 1000 seconds FROM track;");

        parse("INSERT INTO table1 VALUES (1, 2), (3, 4), (5, 6);");
        parse("INSERT INTO table1 (a, b) VALUES ('foo' || 'bar', 2);");
        parse("INSERT INTO table1 SELECT * FROM foo;");

        parse("CREATE TABLE test (
            foo     INT CONSTRAINT pk PRIMARY KEY,
            bar     VARCHAR(256),
            data    BYTE[32] NULL UNIQUE
        );");
    }
}
