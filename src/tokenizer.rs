use std::fmt;

use crate::parser::{DataType, Value};

// Tokenizer
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    And,
    As,
    Asc,
    Bang,
    BangEqual,
    Bool,
    Comma,
    Create,
    Database,
    Delete,
    Desc,
    Drop,
    Equal,
    Float,
    From,
    Greater,
    GreaterEqual,
    GroupBy,
    StringLiteral(String),
    BoolLiteral(bool),
    IntLiteral(i32),
    FloatLiteral(f32),
    Insert,
    Int,
    Into,
    LeftParen,
    Less,
    LessEqual,
    Limit,
    Offset,
    Or,
    OrderBy,
    RightParen,
    Select,
    SemiColon,
    Set,
    Star,
    Table,
    Text,
    Update,
    Values,
    Where,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::And => write!(f, "and"),
            Token::As => write!(f, "as"),
            Token::Asc => write!(f, "asc"),
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Bool => write!(f, "bool"),
            Token::Comma => write!(f, ","),
            Token::Create => write!(f, "create"),
            Token::Database => write!(f, "database"),
            Token::Delete => write!(f, "delete"),
            Token::Desc => write!(f, "desc"),
            Token::Drop => write!(f, "drop"),
            Token::Equal => write!(f, "="),
            Token::Float => write!(f, "float"),
            Token::From => write!(f, "from"),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::GroupBy => write!(f, "group by"),
            Token::StringLiteral(s) => write!(f, "{s}"),
            Token::BoolLiteral(b) => write!(f, "{b}"),
            Token::IntLiteral(i) => write!(f, "{i}"),
            Token::FloatLiteral(fl) => write!(f, "{fl}"),
            Token::Insert => write!(f, "insert"),
            Token::Int => write!(f, "int"),
            Token::Into => write!(f, "into"),
            Token::LeftParen => write!(f, "("),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Limit => write!(f, "limit"),
            Token::Offset => write!(f, "offset"),
            Token::Or => write!(f, "or"),
            Token::OrderBy => write!(f, "order by"),
            Token::RightParen => write!(f, ")"),
            Token::Select => write!(f, "select"),
            Token::SemiColon => write!(f, ";"),
            Token::Set => write!(f, "set"),
            Token::Star => write!(f, "*"),
            Token::Table => write!(f, "table"),
            Token::Text => write!(f, "text"),
            Token::Update => write!(f, "update"),
            Token::Values => write!(f, "values"),
            Token::Where => write!(f, "where"),
        }
    }
}

impl Token {
    /// Return string representation of token if is an identifier
    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            Token::StringLiteral(s) => Some(s),
            _ => None,
        }
    }

    pub fn is_data_type(&self) -> bool {
        matches!(self, Token::Int | Token::Text | Token::Float)
    }

    pub fn to_data_type(&self) -> Result<DataType, String> {
        match self {
            Token::Int => Ok(DataType::Int),
            Token::Text => Ok(DataType::Text),
            Token::Float => Ok(DataType::Float),
            Token::Bool => Ok(DataType::Bool),
            other => Err(format!("Unknown data type: {other:?}")),
        }
    }

    pub fn to_value(&self) -> Result<Value, String> {
        match self {
            Token::StringLiteral(t) => Ok(Value::Text(t.to_string())),
            Token::FloatLiteral(f) => Ok(Value::Float(*f)),
            Token::IntLiteral(i) => Ok(Value::Int(*i)),
            Token::BoolLiteral(b) => Ok(Value::Bool(*b)),
            _ => Err("Invalid token type for to_value conversion".to_string()),
        }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut input = input.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(c) = input.next() {
        match c {
            '*' => tokens.push(Token::Star),
            ';' => tokens.push(Token::SemiColon),
            ',' => tokens.push(Token::Comma),
            '(' => tokens.push(Token::LeftParen),
            ')' => tokens.push(Token::RightParen),
            '=' => tokens.push(Token::Equal),
            '!' => {
                if let Some(next) = input.peek()
                    && matches!(next, '=')
                {
                    tokens.push(Token::BangEqual);
                    input.next();
                } else {
                    tokens.push(Token::Bang)
                }
            }
            '<' => {
                if let Some(next) = input.peek()
                    && matches!(next, '=')
                {
                    tokens.push(Token::LessEqual);
                    input.next();
                } else {
                    tokens.push(Token::Less)
                }
            }
            '>' => {
                if let Some(next) = input.peek()
                    && matches!(next, '=')
                {
                    tokens.push(Token::GreaterEqual);
                    input.next();
                } else {
                    tokens.push(Token::Greater)
                }
            }
            '\'' => {
                let mut ident = String::new();
                while let Some(c) = input.next_if(|cc| *cc != '\'') {
                    ident.extend(c.to_lowercase());
                }
                input.next();
                tokens.push(Token::StringLiteral(ident))
            }
            c if c.is_whitespace() => continue,
            c if c.is_alphanumeric() => {
                let mut parse_next_ident = |current: Option<char>| -> String {
                    let mut ident = match current {
                        Some(cur) => String::from(cur),
                        None => {
                            if input.peek().is_some_and(|c| c.is_whitespace()) {
                                input.next();
                            }
                            String::new()
                        }
                    };
                    while let Some(cc) = input.next_if(|cc| cc.is_alphanumeric() || *cc == '_') {
                        ident.extend(cc.to_lowercase());
                    }
                    ident.to_lowercase()
                };

                match parse_next_ident(Some(c)).as_str() {
                    "select" => tokens.push(Token::Select),
                    "as" => tokens.push(Token::As),
                    "from" => tokens.push(Token::From),
                    "table" => tokens.push(Token::Table),
                    "database" => tokens.push(Token::Database),
                    "drop" => tokens.push(Token::Drop),
                    "create" => tokens.push(Token::Create),
                    "int" | "integer" => tokens.push(Token::Int),
                    "text" => tokens.push(Token::Text),
                    "float" => tokens.push(Token::Float),
                    "bool" => tokens.push(Token::Bool),
                    "into" => tokens.push(Token::Into),
                    "insert" => tokens.push(Token::Insert),
                    "values" => tokens.push(Token::Values),
                    "update" => tokens.push(Token::Update),
                    "where" => tokens.push(Token::Where),
                    "set" => tokens.push(Token::Set),
                    "and" => tokens.push(Token::And),
                    "or" => tokens.push(Token::Or),
                    "limit" => tokens.push(Token::Limit),
                    "offset" => tokens.push(Token::Offset),
                    "delete" => tokens.push(Token::Delete),
                    "asc" => tokens.push(Token::Asc),
                    "desc" => tokens.push(Token::Desc),
                    "order" => match parse_next_ident(None).as_str() {
                        "by" => tokens.push(Token::OrderBy),
                        other => {
                            return Err(format!("Unexpected ident after 'order': {other}"));
                        }
                    },
                    "group" => match parse_next_ident(None).as_str() {
                        "by" => tokens.push(Token::GroupBy),
                        other => return Err(format!("Unexpected ident after 'group': {other}")),
                    },

                    other => {
                        if let Ok(int) = other.parse::<i32>() {
                            tokens.push(Token::IntLiteral(int))
                        } else if let Ok(float) = other.parse::<f32>() {
                            tokens.push(Token::FloatLiteral(float))
                        } else if matches!(other, "true" | "false") {
                            tokens.push(Token::BoolLiteral(other == "true"))
                        } else {
                            tokens.push(Token::StringLiteral(other.to_string()))
                        }
                    }
                }
            }

            _ => return Err(format!("Unexpected character: {c}")),
        }
    }

    Ok(tokens)
}
