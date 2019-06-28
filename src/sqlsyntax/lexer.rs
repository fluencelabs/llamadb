use std::error::Error;
use std::fmt;

/// Disclaimer: The lexer is basically spaghetti. What did you expect?

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Words
    Select,
    From,
    Where,
    Group,
    Having,
    By,
    Limit,
    Distinct,
    Order,
    Asc,
    Desc,
    As,
    Join,
    Inner,
    Outer,
    Left,
    Right,
    On,
    Insert,
    Into,
    Values,
    Update,
    Delete,
    Create,
    Table,
    Index,
    Constraint,
    Primary,
    Key,
    Unique,
    References,
    And,
    Or,
    Between,
    In,
    Is,
    Not,
    Null,
    Explain,
    Truncate,
    Set,
    Drop,

    // Non-letter tokens
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    Plus,
    Minus,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Dot,
    Comma,
    Semicolon,

    Ampersand,
    Pipe,
    ForwardSlash,

    /// ||, the concatenate operator
    DoublePipe,

    /// *, the wildcard for SELECT
    Asterisk,

    /// ?, the prepared statement placeholder
    PreparedStatementPlaceholder,

    // Tokens with values
    Number(String),
    Ident(String),
    StringLiteral(String),
}

fn character_to_token(c: char) -> Option<Token> {
    use self::Token::*;

    Some(match c {
        '=' => Equal,
        '<' => LessThan,
        '>' => GreaterThan,
        '+' => Plus,
        '-' => Minus,
        '(' => LeftParen,
        ')' => RightParen,
        '[' => LeftBracket,
        ']' => RightBracket,
        '.' => Dot,
        ',' => Comma,
        ';' => Semicolon,
        '&' => Ampersand,
        '|' => Pipe,
        '*' => Asterisk,
        '/' => ForwardSlash,
        '?' => PreparedStatementPlaceholder,
        _ => return None,
    })
}

fn word_to_token(word: String) -> Token {
    use self::Token::*;

    // Make all letters lowercase for comparison
    let word_cmp: String = word.chars().flat_map(|c| c.to_lowercase()).collect();

    match word_cmp.as_ref() {
        "select" => Select,
        "from" => From,
        "where" => Where,
        "group" => Group,
        "having" => Having,
        "by" => By,
        "limit" => Limit,
        "distinct" => Distinct,
        "order" => Order,
        "asc" => Asc,
        "desc" => Desc,
        "as" => As,
        "join" => Join,
        "inner" => Inner,
        "outer" => Outer,
        "left" => Left,
        "right" => Right,
        "on" => On,
        "insert" => Insert,
        "into" => Into,
        "values" => Values,
        "update" => Update,
        "delete" => Delete,
        "create" => Create,
        "table" => Table,
        "index" => Index,
        "constraint" => Constraint,
        "primary" => Primary,
        "key" => Key,
        "unique" => Unique,
        "references" => References,
        "and" => And,
        "or" => Or,
        "between" => Between,
        "in" => In,
        "is" => Is,
        "not" => Not,
        "null" => Null,
        "explain" => Explain,
        "truncate" => Truncate,
        "set" => Set,
        "drop" => Drop,
        _ => Ident(word),
    }
}

enum LexerState {
    NoState,
    Word,
    Backtick,
    Apostrophe {
        escaping: bool,
    },
    Number {
        decimal: bool,
    },
    /// Disambiguate an operator sequence.
    OperatorDisambiguate {
        first: char,
    },
    LineComment,
    BlockComment {
        was_prev_char_asterisk: bool,
    },
}

#[derive(PartialEq, Debug)]
pub struct LexerError(String);

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Lexer error: {}", self.0)
    }
}

impl Error for LexerError {}

pub struct Lexer {
    pub tokens: Vec<Token>,

    state: LexerState,
    string_buffer: String,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            tokens: Vec::new(),
            state: LexerState::NoState,
            string_buffer: String::new(),
        }
    }

    pub fn is_no_state(&self) -> bool {
        match self.state {
            LexerState::NoState => true,
            _ => false,
        }
    }

    fn no_state(&mut self, char: char) -> Result<LexerState, LexerError> {
        match char {
            'a'...'z' | 'A'...'Z' | '_' => {
                self.string_buffer.push(char);
                Ok(LexerState::Word)
            }
            '`' => Ok(LexerState::Backtick),
            '\'' => {
                // string literal
                Ok(LexerState::Apostrophe { escaping: false })
            }
            '0'...'9' => {
                self.string_buffer.push(char);
                Ok(LexerState::Number { decimal: false })
            }
            ' ' | '\t' | '\n' => {
                // whitespace
                Ok(LexerState::NoState)
            }
            c => {
                use self::Token::*;

                match character_to_token(c) {
                    Some(LessThan) | Some(GreaterThan) | Some(Minus) | Some(Pipe)
                    | Some(ForwardSlash) => Ok(LexerState::OperatorDisambiguate { first: c }),
                    Some(token) => {
                        self.tokens.push(token);
                        Ok(LexerState::NoState)
                    }
                    None => Err(LexerError(format!("Unknown character {}", c))),
                }
            }
        }
    }

    /// Clears `self.string_buffer` and returns old value.
    fn move_string_buffer(&mut self) -> String {
        use std::mem;
        mem::replace(&mut self.string_buffer, String::new())
    }

    pub fn feed_character(&mut self, char: Option<char>) -> Result<(), LexerError> {
        let result_state = match self.state {
            LexerState::NoState => match char {
                Some(c) => self.no_state(c),
                None => Ok(LexerState::NoState),
            },
            LexerState::Word => match char {
                Some(c) => match c {
                    'a'...'z' | 'A'...'Z' | '_' | '0'...'9' => {
                        self.string_buffer.push(c);
                        Ok(LexerState::Word)
                    }
                    c => {
                        let buffer = self.move_string_buffer();
                        self.tokens.push(word_to_token(buffer));
                        self.no_state(c)
                    }
                },
                None => {
                    let buffer = self.move_string_buffer();
                    self.tokens.push(word_to_token(buffer));
                    Ok(LexerState::NoState)
                }
            },
            LexerState::Backtick => match char {
                Some('`') => {
                    let buffer = self.move_string_buffer();
                    self.tokens.push(Token::Ident(buffer));
                    Ok(LexerState::NoState)
                }
                Some(c) => {
                    self.string_buffer.push(c);
                    Ok(LexerState::Backtick)
                }
                None => Err(LexerError(
                    "Backtick didn't finish. Something do wrong with ` character".to_string(),
                )),
            },
            LexerState::Apostrophe { escaping } => {
                if let Some(c) = char {
                    match (escaping, c) {
                        (false, '\'') => {
                            // unescaped apostrophe
                            let buffer = self.move_string_buffer();
                            self.tokens.push(Token::StringLiteral(buffer));
                            Ok(LexerState::NoState)
                        }
                        (false, '\\') => {
                            // unescaped backslash
                            Ok(LexerState::Apostrophe { escaping: true })
                        }
                        (true, _) | _ => {
                            self.string_buffer.push(c);
                            Ok(LexerState::Apostrophe { escaping: false })
                        }
                    }
                } else {
                    // Apostrophe didn't not finish.
                    Err(LexerError(
                        "Apostrophe didn't finish. Something do wrong with ' character".to_string(),
                    ))
                }
            }
            LexerState::Number { decimal } => {
                if let Some(c) = char {
                    match c {
                        '0'...'9' => {
                            self.string_buffer.push(c);
                            Ok(LexerState::Number { decimal })
                        }
                        '.' if !decimal => {
                            // Add a decimal point. None has been added yet.
                            self.string_buffer.push(c);
                            Ok(LexerState::Number { decimal: true })
                        }
                        c => {
                            let buffer = self.move_string_buffer();
                            self.tokens.push(Token::Number(buffer));
                            self.no_state(c)
                        }
                    }
                } else {
                    let buffer = self.move_string_buffer();
                    self.tokens.push(Token::Number(buffer));
                    Ok(LexerState::NoState)
                }
            }
            LexerState::OperatorDisambiguate { first } => {
                use self::Token::*;

                if let Some(c) = char {
                    match (first, c) {
                        ('<', '>') => {
                            self.tokens.push(NotEqual);
                            Ok(LexerState::NoState)
                        }
                        ('<', '=') => {
                            self.tokens.push(LessThanOrEqual);
                            Ok(LexerState::NoState)
                        }
                        ('>', '=') => {
                            self.tokens.push(GreaterThanOrEqual);
                            Ok(LexerState::NoState)
                        }
                        ('|', '|') => {
                            self.tokens.push(DoublePipe);
                            Ok(LexerState::NoState)
                        }
                        ('-', '-') => Ok(LexerState::LineComment),
                        ('/', '*') => Ok(LexerState::BlockComment {
                            was_prev_char_asterisk: false,
                        }),
                        _ => {
                            character_to_token(first).map(|c| self.tokens.push(c));
                            self.no_state(c)
                        }
                    }
                } else {
                    character_to_token(first).map(|c| self.tokens.push(c));
                    Ok(LexerState::NoState)
                }
            }
            LexerState::LineComment => match char {
                Some('\n') => Ok(LexerState::NoState),
                _ => Ok(LexerState::LineComment),
            },
            LexerState::BlockComment {
                was_prev_char_asterisk,
            } => {
                if was_prev_char_asterisk && char == Some('/') {
                    Ok(LexerState::NoState)
                } else {
                    Ok(LexerState::BlockComment {
                        was_prev_char_asterisk: char == Some('*'),
                    })
                }
            }
        };

        Ok(self.state = result_state?)
    }

    pub fn feed_characters<I>(&mut self, iter: I) -> Result<(), LexerError>
    where
        I: Iterator<Item = char>,
    {
        for c in iter {
            self.feed_character(Some(c))?;
        }

        Ok(())
    }
}

pub fn parse(sql: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new();

    lexer.feed_characters(sql.chars())?;
    lexer.feed_character(None)?;

    Ok(lexer.tokens)
}

#[cfg(test)]
mod test {
    use super::parse;
    use sqlsyntax::lexer::LexerError;

    fn id(value: &str) -> super::Token {
        super::Token::Ident(value.to_string())
    }

    fn number(value: &str) -> super::Token {
        super::Token::Number(value.to_string())
    }

    #[test]
    fn test_sql_lexer_dontconfuseidentswithkeywords() {
        use super::Token::*;
        // Not: AS, Ident("df")
        assert_eq!(parse("asdf").unwrap(), vec![Ident("asdf".to_string())]);
    }

    #[test]
    fn test_sql_lexer_escape() {
        use super::Token::*;
        // Escaped apostrophe
        assert_eq!(
            parse(r"'\''").unwrap(),
            vec![StringLiteral("'".to_string())]
        );
    }

    #[test]
    fn test_sql_lexer_numbers() {
        use super::Token::*;

        assert_eq!(parse("12345").unwrap(), vec![number("12345")]);
        assert_eq!(parse("0.25").unwrap(), vec![number("0.25")]);
        assert_eq!(
            parse("0.25 + -0.25").unwrap(),
            vec![number("0.25"), Plus, Minus, number("0.25")]
        );
        assert_eq!(
            parse("-0.25 + 0.25").unwrap(),
            vec![Minus, number("0.25"), Plus, number("0.25")]
        );
        assert_eq!(
            parse("- 0.25 - -0.25").unwrap(),
            vec![Minus, number("0.25"), Minus, Minus, number("0.25")]
        );
        assert_eq!(parse("- 0.25 --0.25").unwrap(), vec![Minus, number("0.25")]);
        assert_eq!(
            parse("0.25 -0.25").unwrap(),
            vec![number("0.25"), Minus, number("0.25")]
        );
    }

    #[test]
    fn test_sql_lexer_query1() {
        use super::Token::*;

        assert_eq!(parse(" SeLECT a,    b as alias1 , c alias2, d ` alias three ` fRoM table1 WHERE a='Hello World'; ").unwrap(),
            vec![
                Select, id("a"), Comma, id("b"), As, id("alias1"), Comma,
                id("c"), id("alias2"), Comma, id("d"), id(" alias three "),
                From, id("table1"),
                Where, id("a"), Equal, StringLiteral("Hello World".to_string()), Semicolon
            ]
        );
    }

    #[test]
    fn test_sql_lexer_query2() {
        use super::Token::*;

        let query = r"
        -- Get employee count from each department
        SELECT d.id departmentId, count(e.id) employeeCount
        FROM department d
        LEFT JOIN employee e ON e.departmentId = d.id
        GROUP BY departmentId;
        ";

        assert_eq!(
            parse(query).unwrap(),
            vec![
                Select,
                id("d"),
                Dot,
                id("id"),
                id("departmentId"),
                Comma,
                id("count"),
                LeftParen,
                id("e"),
                Dot,
                id("id"),
                RightParen,
                id("employeeCount"),
                From,
                id("department"),
                id("d"),
                Left,
                Join,
                id("employee"),
                id("e"),
                On,
                id("e"),
                Dot,
                id("departmentId"),
                Equal,
                id("d"),
                Dot,
                id("id"),
                Group,
                By,
                id("departmentId"),
                Semicolon
            ]
        );
    }

    #[test]
    fn test_sql_lexer_operators() {
        use super::Token::*;

        assert_eq!(
            parse("> = >=< =><").unwrap(),
            vec![
                GreaterThan,
                Equal,
                GreaterThanOrEqual,
                LessThan,
                Equal,
                GreaterThan,
                LessThan
            ]
        );

        assert_eq!(
            parse(" ><>> >< >").unwrap(),
            vec![
                GreaterThan,
                NotEqual,
                GreaterThan,
                GreaterThan,
                LessThan,
                GreaterThan
            ]
        );
    }

    #[test]
    fn test_sql_lexer_blockcomment() {
        use super::Token::*;

        assert_eq!(
            parse("hello/*/a/**/,/*there, */world").unwrap(),
            vec![id("hello"), Comma, id("world")]
        );

        assert_eq!(
            parse("/ */").unwrap(),
            vec![ForwardSlash, Asterisk, ForwardSlash]
        );

        assert_eq!(parse("/**/").unwrap(), vec![]);

        assert_eq!(
            parse("a/* test\ntest** /\nb*/c").unwrap(),
            vec![id("a"), id("c")]
        );
    }

    #[test]
    fn test_sql_lexer_invalid_char() {
        match parse("🌠") {
            Ok(_) => panic!("Expected error!"),
            Err(err) => {
                assert_eq!(err, LexerError("Unknown character 🌠".to_string()));
            }
        }
        match parse("`") {
            Ok(_) => panic!("Expected error!"),
            Err(err) => {
                assert_eq!(
                    err,
                    LexerError(
                        "Backtick didn\'t finish. Something do wrong with ` character".to_string()
                    )
                );
            }
        }

        match parse("'") {
            Ok(_) => panic!("Expected error!"),
            Err(err) => {
                assert_eq!(
                    err,
                    LexerError(
                        "Apostrophe didn\'t finish. Something do wrong with ' character"
                            .to_string()
                    )
                );
            }
        }
    }

}
