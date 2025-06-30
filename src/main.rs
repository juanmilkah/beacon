/*
 * DATABASE ARCHITECTURE DOCUMENTATION
 *
 * This is a complete database implementation built from scratch in Rust.
 * The system follows a traditional database architecture with five main layers:
 *
 * 1. TOKENIZER
 *    - Takes raw SQL text input
 *    - Breaks it into meaningful tokens (keywords, identifiers, operators, literals)
 *    - Example: "SELECT name FROM users" -> [SELECT, name, FROM, users]
 *    - Handles whitespace, comments, and syntax validation at character level
 *
 * 2. PARSER
 *    - Takes tokens from tokenizer
 *    - Builds Abstract Syntax Tree (AST) representing SQL structure
 *    - Validates SQL grammar and syntax rules
 *    - Example: SELECT statement becomes tree with columns, table, conditions
 *    - Reports syntax errors with helpful messages
 *
 * 3. COMPILER
 *    - Takes AST from parser
 *    - Converts high-level SQL operations into low-level bytecode instructions
 *    - Performs query optimization and planning
 *    - Example: SELECT becomes series of LoadTable, LoadColumn, Select opcodes
 *    - Handles type checking and semantic validation
 *
 * 4. BYTECODE ENGINE (Virtual Machine)
 *    - Executes compiled bytecode instructions
 *    - Stack-based architecture for operand management
 *    - Implements all SQL operations (CREATE, INSERT, SELECT, UPDATE, DELETE)
 *    - Manages program counter and instruction flow
 *    - Returns query results or confirmation of data modifications
 *
 * 5. STORAGE LAYER
 *    - Handles persistent data storage and retrieval
 *    - Manages databases, tables, rows, and columns
 *    - Implements data types (Text, Int, Float, Bool)
 *    - Provides CRUD operations for the bytecode engine
 *    - Currently in-memory but designed for disk persistence
 *
 * DATA FLOW:
 * SQL Text -> Tokenizer -> Tokens -> Parser -> AST -> Compiler -> Bytecode -> VM -> Storage
 *
 * This layered approach provides:
 * - Clean separation of concerns
 * - Easy testing and debugging of individual components
 * - Extensibility for new SQL features
 * - Performance optimization at each layer
 * - Standard database architecture patterns
 */

use std::{collections::HashMap, fmt};

// Ast generator
#[derive(Debug, Clone, PartialEq)]
enum Token {
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
    fn as_identifier(&self) -> Option<&str> {
        match self {
            Token::StringLiteral(s) => Some(s),
            _ => None,
        }
    }

    fn is_data_type(&self) -> bool {
        matches!(self, Token::Int | Token::Text | Token::Float)
    }

    fn to_data_type(&self) -> Result<DataType, String> {
        match self {
            Token::Int => Ok(DataType::Int),
            Token::Text => Ok(DataType::Text),
            Token::Float => Ok(DataType::Float),
            Token::Bool => Ok(DataType::Bool),
            other => Err(format!("Unknown data type: {other:?}")),
        }
    }

    fn to_value(&self) -> Result<Value, String> {
        match self {
            Token::StringLiteral(t) => Ok(Value::Text(t.to_string())),
            Token::FloatLiteral(f) => Ok(Value::Float(*f)),
            Token::IntLiteral(i) => Ok(Value::Int(*i)),
            Token::BoolLiteral(b) => Ok(Value::Bool(*b)),
            _ => Err("Invalid token type for to_value conversion".to_string()),
        }
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
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

/// Ast Representation
#[derive(Debug, PartialEq)]
enum Statement {
    Select(SelectStatement),
    Create(CreateStatement),
    Drop(DropStatement),
    Insert(InsertStatement),
    Update(UpdateStatement),
    Delete(DeleteStatement),
}

#[derive(Debug, PartialEq)]
struct DeleteStatement {
    core: DeleteCore,
}

#[derive(Debug, PartialEq)]
struct DeleteCore {
    table: String,
    conditions: Option<Vec<MultiCondition>>, // TODO! does this handle multiple
}

#[derive(Debug, PartialEq)]
struct UpdateStatement {
    core: UpdateCore,
}

#[derive(Debug, PartialEq)]
struct UpdateCore {
    table: String,
    columns: Vec<ColumnExpr>,
    conditions: Option<Vec<MultiCondition>>,
}

#[derive(Debug, PartialEq)]
enum Condition {
    Single(SingleCondition),
    Double(DoubleCondition),
    Multi(MultiCondition),
}

#[derive(Debug, PartialEq)]
struct SingleCondition {
    name: Token,
}

#[derive(Debug, PartialEq)]
struct DoubleCondition {
    name: Token,
    value: Value,
}

#[derive(Debug, PartialEq)]
struct MultiCondition {
    column: String,
    criterion: Criterion,
    value: Value,
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Text(String),
    Int(i32),
    Bool(bool),
    Float(f32),
}

#[derive(Debug, PartialEq)]
enum Criterion {
    GreaterThan,
    EqualTo,
    LessThan,
    GreaterEqualTo,
    LessEqualTo,
    NotEqualTo,
}

#[derive(Debug, PartialEq)]
struct InsertStatement {
    core: InsertCore,
}

#[derive(Debug, PartialEq)]
struct InsertCore {
    table: String,
    columns: Vec<ColumnExpr>,
    values: Vec<Vec<Value>>,
}

#[derive(Debug, PartialEq)]
struct CreateStatement {
    core: CreateCore,
}

#[derive(Debug, PartialEq)]
struct CreateCore {
    family: CreateFamily,
}

#[derive(Debug, PartialEq)]
enum CreateFamily {
    Database(CreateDatabase),
    Table(CreateTable),
}

#[derive(Debug, PartialEq)]
struct CreateTable {
    name: String,
    columns: Vec<ColumnExpr>,
}

#[derive(Debug, PartialEq)]
struct CreateDatabase {
    name: String,
}

#[derive(Debug, PartialEq)]
struct DropStatement {
    core: DropCore,
}

#[derive(Debug, PartialEq)]
struct DropCore {
    family: DropFamily,
}

#[derive(Debug, PartialEq)]
enum DropFamily {
    Database(DropDatabase),
    Table(DropTable),
}

#[derive(Debug, PartialEq)]
struct DropTable {
    name: String,
}

#[derive(Debug, PartialEq)]
struct DropDatabase {
    name: String,
}

#[derive(Debug, PartialEq)]
struct SelectStatement {
    core: SelectCore,
}

#[derive(Debug, PartialEq)]
struct SelectCore {
    result_columns: Vec<ResultColumn>,
    from: SelectFrom,
    conditions: Option<Vec<Condition>>,
}

#[derive(Debug, PartialEq)]
enum SelectFrom {
    Table(String),
}

#[derive(Debug, PartialEq)]
enum ResultColumn {
    Star,
    Expr(AliasedColumn),
}

#[derive(Debug, PartialEq)]
struct AliasedColumn {
    expr: ColumnExpr,
    alias: Option<String>,
}

#[derive(Debug, PartialEq, Clone)]
enum ColumnExpr {
    Name(ColumnName),
    Typed(TypedColumn),
    Assignment(ValueAssignment),
}

#[derive(Debug, PartialEq, Clone)]
struct ValueAssignment {
    name: String,
    value: Value,
}

#[derive(Debug, PartialEq, Clone)]
struct ColumnName {
    name: String,
}

#[derive(Debug, PartialEq, Clone)]
struct TypedColumn {
    col_name: String,
    data_type: DataType,
}

#[derive(Debug, PartialEq, Clone)]
enum DataType {
    Int,
    Float,
    Text,
    Bool,
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::Int => write!(f, "Int"),
            DataType::Float => write!(f, "Float"),
            DataType::Text => write!(f, "Text"),
            DataType::Bool => write!(f, "Bool"),
        }
    }
}

/// Recursive Descent parser
#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    pos: usize, // current pointer position
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn next_token_is(&self, expected: Token) -> bool {
        self.tokens.get(self.pos) == Some(&expected)
    }

    fn next_token(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        if token.is_some() {
            self.pos += 1; // advance pointer
        }
        token
    }

    fn peek_next_token(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect_matching(&mut self, f: impl FnOnce(&Token) -> bool) -> Result<&Token, String> {
        match self.next_token() {
            Some(token) if f(token) => Ok(token),
            Some(token) => Err(format!("Unexpected token: {token:?}")),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    fn expect_eq(&mut self, expected: Token) -> Result<&Token, String> {
        self.expect_matching(|t| *t == expected)
            .map_err(|err| format!("{err}, Expected: {expected:?}"))
    }

    /// Get string representation of ident; fail if token is not ident
    fn expect_identifier(&mut self) -> Result<&str, String> {
        self.expect_matching(|t| matches!(t, Token::StringLiteral(_)))
            .map(|t| t.as_identifier().unwrap())
    }

    fn expect_value(&mut self) -> Result<Value, String> {
        self.expect_matching(|t| {
            matches!(
                t,
                Token::StringLiteral(_)
                    | Token::BoolLiteral(_)
                    | Token::IntLiteral(_)
                    | Token::FloatLiteral(_)
            )
        })
        .map(|t| t.to_value())?
    }

    fn parse_expr(&mut self) -> Result<ColumnExpr, String> {
        let name = self.expect_identifier()?.to_string();
        match self.peek_next_token() {
            None => Err("Unexpected end of input".to_string()),
            Some(token) => {
                if token.is_data_type() {
                    let data_type = token.to_data_type().unwrap();
                    self.advance();
                    Ok(ColumnExpr::Typed(TypedColumn {
                        col_name: name,
                        data_type,
                    }))
                } else {
                    Ok(ColumnExpr::Name(ColumnName { name }))
                }
            }
        }
    }

    fn parse_expr_result_column(&mut self) -> Result<AliasedColumn, String> {
        let expr = self.parse_expr()?;
        let alias = if self.next_token_is(Token::As) {
            self.advance();
            Some(self.expect_identifier()?.to_string())
        } else {
            None
        };

        Ok(AliasedColumn { expr, alias })
    }

    fn parse_result_column(&mut self) -> Result<ResultColumn, String> {
        if self.next_token_is(Token::Star) {
            self.advance();
            return Ok(ResultColumn::Star);
        }

        Ok(ResultColumn::Expr(self.parse_expr_result_column()?))
    }

    fn parse_result_columns(&mut self) -> Result<Vec<ResultColumn>, String> {
        let mut columns = vec![self.parse_result_column()?];

        while self.next_token_is(Token::Comma) {
            self.advance();
            columns.push(self.parse_result_column()?);
        }

        Ok(columns)
    }

    fn parse_select_from(&mut self) -> Result<SelectFrom, String> {
        let table = self.expect_identifier()?;
        Ok(SelectFrom::Table(table.to_string()))
    }

    fn parse_single_condition(&mut self) -> Result<SingleCondition, String> {
        let name = self.next_token().unwrap().to_owned();
        Ok(SingleCondition { name })
    }

    fn parse_double_condition(&mut self) -> Result<DoubleCondition, String> {
        let name = self.next_token().unwrap().to_owned();
        let value = self.expect_value()?;

        Ok(DoubleCondition { name, value })
    }

    fn parse_select_conditions(&mut self) -> Result<Option<Vec<Condition>>, String> {
        let mut conds = Vec::new();

        while let Some(next) = self.peek_next_token() {
            let cond = match next {
                Token::Asc | Token::Desc => Condition::Single(self.parse_single_condition()?),
                Token::Offset | Token::Limit | Token::OrderBy | Token::GroupBy => {
                    Condition::Double(self.parse_double_condition()?)
                }
                Token::Where => Condition::Multi(self.parse_multi_condition()?),
                _ => break,
            };

            conds.push(cond);
        }

        if conds.is_empty() {
            Ok(None)
        } else {
            Ok(Some(conds))
        }
    }

    fn parse_select(&mut self) -> Result<SelectStatement, String> {
        self.expect_eq(Token::Select)?;
        let result_columns = self.parse_result_columns()?;
        self.expect_eq(Token::From)?;
        let from = self.parse_select_from()?;
        let conditions = self.parse_select_conditions()?;

        Ok(SelectStatement {
            core: SelectCore {
                result_columns,
                from,
                conditions,
            },
        })
    }

    fn parse_drop_family(&mut self) -> Result<DropFamily, String> {
        let token = self.next_token();
        match token {
            Some(token) => match token {
                Token::Database => {
                    let name = self.expect_identifier()?;
                    Ok(DropFamily::Database(DropDatabase {
                        name: name.to_string(),
                    }))
                }
                Token::Table => {
                    let name = self.expect_identifier()?;
                    Ok(DropFamily::Table(DropTable {
                        name: name.to_string(),
                    }))
                }
                other => Err(format!("Unexpected token: {other:?}")),
            },
            None => Err("Unexpected end of input".to_string()),
        }
    }

    fn parse_drop(&mut self) -> Result<DropStatement, String> {
        self.expect_eq(Token::Drop)?;
        let family = self.parse_drop_family()?;
        Ok(DropStatement {
            core: DropCore { family },
        })
    }

    fn parse_create_table_columns(&mut self) -> Result<Vec<ColumnExpr>, String> {
        let mut columns = vec![self.parse_expr()?];

        while self.next_token_is(Token::Comma) {
            self.advance();
            columns.push(self.parse_expr()?)
        }

        Ok(columns)
    }

    fn parse_create_table(&mut self) -> Result<CreateTable, String> {
        let name = self.expect_identifier()?.to_string();
        self.expect_eq(Token::LeftParen)?;
        let columns = self.parse_create_table_columns()?;
        self.expect_eq(Token::RightParen)?;

        Ok(CreateTable { name, columns })
    }

    fn parse_create_database(&mut self) -> Result<CreateDatabase, String> {
        let name = self.expect_identifier()?;
        Ok(CreateDatabase {
            name: name.to_string(),
        })
    }

    fn parse_create_family(&mut self) -> Result<CreateFamily, String> {
        match self.next_token() {
            None => Err("Unexpected end of input".to_string()),
            Some(token) => match token {
                Token::Database => Ok(CreateFamily::Database(self.parse_create_database()?)),
                Token::Table => Ok(CreateFamily::Table(self.parse_create_table()?)),
                other => Err(format!("Unexpected create family token: {other:?}")),
            },
        }
    }

    fn parse_create(&mut self) -> Result<CreateStatement, String> {
        self.expect_eq(Token::Create)?;
        let family = self.parse_create_family()?;
        Ok(CreateStatement {
            core: CreateCore { family },
        })
    }

    fn parse_insert_value(&mut self) -> Result<Vec<Value>, String> {
        let mut vals = vec![self.expect_value()?];

        while self.next_token_is(Token::Comma) {
            self.advance();
            vals.push(self.expect_value()?);
        }

        Ok(vals)
    }

    fn parse_insert_values(&mut self) -> Result<Vec<Vec<Value>>, String> {
        self.expect_eq(Token::LeftParen)?;
        let mut vals = vec![self.parse_insert_value()?];
        self.expect_eq(Token::RightParen)?;

        while self.next_token_is(Token::Comma) {
            self.advance();
            self.expect_eq(Token::LeftParen)?;
            vals.push(self.parse_insert_value()?);
            self.expect_eq(Token::RightParen)?;
        }
        Ok(vals)
    }

    fn parse_insert_columns(&mut self) -> Result<Vec<ColumnExpr>, String> {
        let mut cols = vec![self.parse_expr()?];

        while self.next_token_is(Token::Comma) {
            self.advance();
            cols.push(self.parse_expr()?);
        }

        Ok(cols)
    }

    fn parse_insert_core(&mut self) -> Result<InsertCore, String> {
        let table = self.expect_identifier()?.to_string();
        self.expect_eq(Token::LeftParen)?;
        let columns = self.parse_insert_columns()?;
        self.expect_eq(Token::RightParen)?;
        self.expect_eq(Token::Values)?;
        let values = self.parse_insert_values()?;
        Ok(InsertCore {
            table,
            columns,
            values,
        })
    }

    fn parse_insert(&mut self) -> Result<InsertStatement, String> {
        self.expect_eq(Token::Insert)?;
        self.expect_eq(Token::Into)?;
        let core = self.parse_insert_core()?;
        Ok(InsertStatement { core })
    }

    fn expect_criterion(&mut self) -> Result<Criterion, String> {
        match self.next_token() {
            None => Err("Missing criterion for condition;".to_string()),
            Some(c) => match c {
                Token::Less => Ok(Criterion::LessThan),
                Token::Greater => Ok(Criterion::GreaterThan),
                Token::LessEqual => Ok(Criterion::LessEqualTo),
                Token::GreaterEqual => Ok(Criterion::GreaterEqualTo),
                Token::BangEqual => Ok(Criterion::NotEqualTo),
                Token::Equal => Ok(Criterion::EqualTo),
                other => Err(format!("Expected criterion, found: {other:?}")),
            },
        }
    }

    fn parse_multi_condition(&mut self) -> Result<MultiCondition, String> {
        let column = self.expect_identifier()?.to_string();
        let criterion = self.expect_criterion()?;
        let value = self.expect_value()?;

        Ok(MultiCondition {
            column,
            criterion,
            value,
        })
    }

    fn parse_conditions(&mut self) -> Result<Option<Vec<MultiCondition>>, String> {
        if !matches!(self.peek_next_token(), Some(Token::Where)) {
            return Ok(None);
        }
        self.advance(); // skip where

        let mut conds = Vec::new();
        // TODO! What's the guard for condition?
        loop {
            conds.push(self.parse_multi_condition()?);
            if let Some(next) = self.peek_next_token()
                && matches!(next, Token::And | Token::Or)
            {
                self.advance();
                continue;
            } else {
                break;
            }
        }

        Ok(Some(conds))
    }

    fn parse_update_column(&mut self) -> Result<ColumnExpr, String> {
        let name = self.expect_identifier()?.to_string();
        self.expect_eq(Token::Equal)?;
        let value = self.expect_value()?;

        Ok(ColumnExpr::Assignment(ValueAssignment { name, value }))
    }

    fn parse_update_columns(&mut self) -> Result<Vec<ColumnExpr>, String> {
        let mut cols = vec![self.parse_update_column()?];

        while matches!(self.peek_next_token(), Some(Token::Comma)) {
            self.advance();
            cols.push(self.parse_update_column()?);
        }

        Ok(cols)
    }

    fn parse_update_core(&mut self) -> Result<UpdateCore, String> {
        let table = self.expect_identifier()?.to_string();
        self.expect_eq(Token::Set)?;
        let columns = self.parse_update_columns()?;
        let conditions = self.parse_conditions()?;
        Ok(UpdateCore {
            table,
            columns,
            conditions,
        })
    }

    fn parse_update(&mut self) -> Result<UpdateStatement, String> {
        self.expect_eq(Token::Update)?;
        let core = self.parse_update_core()?;
        Ok(UpdateStatement { core })
    }

    fn parse_delete_core(&mut self) -> Result<DeleteCore, String> {
        let table = self.expect_identifier()?.to_string();
        let conditions = self.parse_conditions()?;

        Ok(DeleteCore { table, conditions })
    }

    fn parse_delete(&mut self) -> Result<DeleteStatement, String> {
        self.expect_eq(Token::Delete)?;
        self.expect_eq(Token::From)?;
        let core = self.parse_delete_core()?;

        Ok(DeleteStatement { core })
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.tokens.first() {
            Some(token) => match token {
                Token::Drop => Ok(Statement::Drop(self.parse_drop()?)),
                Token::Select => Ok(Statement::Select(self.parse_select()?)),
                Token::Create => Ok(Statement::Create(self.parse_create()?)),
                Token::Insert => Ok(Statement::Insert(self.parse_insert()?)),
                Token::Update => Ok(Statement::Update(self.parse_update()?)),
                Token::Delete => Ok(Statement::Delete(self.parse_delete()?)),
                other => Err(format!("Unexpected command token: {other:?}")),
            },
            None => Err("Unexpected end on input".to_string()),
        }
    }
}

// select * from foo;
// select name from foo;
// select name as n from foo;
// select name, age as x from foo;
// select * from foo limit 10;
// select * from foo offset 10;
// SELECT * FROM users LIMIT 10 OFFSET 5;
// SELECT * FROM users ORDER BY created_at DESC;
// SELECT name FROM users ORDER BY age ASC;
// SELECT department, COUNT(*) FROM employees GROUP BY department;
//
//  /* Not Implemented */
// SELECT department, COUNT(*) FROM employees GROUP BY department HAVING COUNT(*) > 5;
// SELECT u.name, o.total FROM users u JOIN orders o ON u.id = o.user_id;
// SELECT * FROM users LEFT JOIN profiles ON users.id = profiles.user_id;
// SELECT * FROM users WHERE id IN (SELECT user_id FROM orders);
// SELECT name FROM users WHERE EXISTS (SELECT 1 FROM logins WHERE logins.user_id = users.id);
// SELECT COUNT(*) FROM users;
// SELECT AVG(age) FROM users;
// SELECT MAX(score) FROM games;
// SELECT DISTINCT city FROM users;
// /* ** */
// drop table foo;
// drop database foo;

// create database foo;
// create table foo(name text);
// create table foo(name text, age int);

// insert into cats(name) values(tracy);
// insert into cats(name, age) values(tracy, 10);
// insert into cats(name, age) values (tracy, 10), (mike, 12);
// insert into cats(name, age) values('mike andrew', 12);

// update cats set age = 20;
// update cats set age = 15 where name = 'mike';
// update cats set name = 'andrew', age = 20 where name = 'mike';
// update cats set name = andrew where name = mike or name = annette;
// update cats set name = andrew where name = mike and name = annette;

// delete from cats;
// delete from cats where name = 'andrew';
// delete from cats where name = 'andrew' and age = 20;

fn parse_statement(input: &str) -> Result<Statement, String> {
    let tokens = tokenize(input)?;
    // dbg!(&tokens);
    let mut parser = Parser::new(tokens);
    let stmt = parser.parse_statement()?;
    parser.expect_eq(Token::SemiColon)?;
    Ok(stmt)
}

// The compiler
#[derive(Debug, Clone, PartialEq)]
enum OpCode {
    CreateTable,
    CreateDatabase,
    DropDatabase,
    DropTable,
    Insert,
    Update,
    Delete,
    Select,
    // stack ops
    LoadTable,
    LoadColumn,
    LoadValue,
    LoadCriterion,
    //
    Filter,
    SetColumn,
    Halt,
}

#[derive(Debug, Clone, PartialEq)]
struct Instruction {
    opcode: OpCode,
    operand: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
struct Compiler {
    instructions: Vec<Instruction>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    fn emit(&mut self, opcode: OpCode, operand: Option<String>) {
        self.instructions.push(Instruction { opcode, operand })
    }

    fn compile_select_statement(&mut self, stmt: &SelectStatement) {
        let core = &stmt.core;

        match &core.from {
            SelectFrom::Table(table) => self.emit(OpCode::LoadValue, Some(table.clone())),
        }

        for col in &core.result_columns {
            match col {
                ResultColumn::Star => self.emit(OpCode::LoadColumn, Some("*".to_string())),
                ResultColumn::Expr(aliased_column) => {
                    self.compile_column_expr(&aliased_column.expr)
                }
            }
        }

        if let Some(conditions) = &core.conditions {
            for condition in conditions {
                match condition {
                    Condition::Single(single_condition) => {
                        self.emit(OpCode::LoadValue, Some(single_condition.name.to_string()));
                    }
                    Condition::Double(double_condition) => {
                        self.emit(OpCode::LoadValue, Some(double_condition.name.to_string()));
                        self.compile_value(&double_condition.value);
                    }
                    Condition::Multi(multi_condition) => {
                        self.compile_multi_condition(multi_condition);
                    }
                }
            }
        }

        self.emit(OpCode::Select, None);
    }

    fn compile_create_statement(&mut self, stmt: &CreateStatement) {
        match &stmt.core.family {
            CreateFamily::Database(create_database) => {
                self.emit(OpCode::LoadValue, Some(create_database.name.clone()));
                self.emit(OpCode::CreateDatabase, None);
            }
            CreateFamily::Table(create_table) => {
                self.emit(OpCode::LoadTable, Some(create_table.name.clone()));

                for col in &create_table.columns {
                    self.compile_column_expr(col);
                }

                self.emit(OpCode::CreateTable, None);
            }
        }
    }

    fn compile_update_statement(&mut self, stmt: &UpdateStatement) {
        let core = &stmt.core;

        for col in &core.columns {
            if let ColumnExpr::Assignment(assignment) = col {
                self.emit(OpCode::LoadColumn, Some(assignment.name.clone()));
                self.compile_value(&assignment.value);
                self.emit(OpCode::SetColumn, None);
            }
        }

        if let Some(conditions) = &core.conditions {
            self.compile_conditions(conditions);
        }

        self.emit(OpCode::Update, None);
    }

    fn compile_drop_statement(&mut self, stmt: &DropStatement) {
        match &stmt.core.family {
            DropFamily::Database(drop_database) => {
                self.emit(OpCode::LoadValue, Some(drop_database.name.clone()));
                self.emit(OpCode::DropDatabase, None);
            }
            DropFamily::Table(drop_table) => {
                self.emit(OpCode::LoadValue, Some(drop_table.name.clone()));
                self.emit(OpCode::DropTable, None);
            }
        }
    }

    fn compile_column_expr(&mut self, col: &ColumnExpr) {
        match col {
            ColumnExpr::Name(column_name) => {
                self.emit(OpCode::LoadColumn, Some(column_name.name.clone()))
            }
            ColumnExpr::Typed(typed_column) => {
                // flipped these
                self.emit(OpCode::LoadValue, Some(typed_column.data_type.to_string()));
                self.emit(OpCode::LoadColumn, Some(typed_column.col_name.clone()));
            }
            ColumnExpr::Assignment(value_assignment) => {
                self.emit(OpCode::LoadColumn, Some(value_assignment.name.clone()));
                self.compile_value(&value_assignment.value);
            }
        }
    }

    fn compile_insert_statement(&mut self, stmt: &InsertStatement) {
        let core = &stmt.core;

        self.emit(OpCode::LoadTable, Some(core.table.clone()));

        for col in &core.columns {
            self.compile_column_expr(col);
        }

        for row in &core.values {
            for val in row {
                self.compile_value(val);
            }
        }

        self.emit(OpCode::Insert, None);
    }

    fn compile_value(&mut self, value: &Value) {
        let v = match value {
            Value::Text(t) => format!("Text:{t}"),
            Value::Int(i) => format!("Int:{i}"),
            Value::Bool(b) => format!("Bool:{b}"),
            Value::Float(f) => format!("Float:{f}"),
        };

        self.emit(OpCode::LoadValue, Some(v))
    }

    fn compile_criterion(&mut self, criterion: &Criterion) {
        let cr = match criterion {
            Criterion::GreaterThan => ">",
            Criterion::EqualTo => "=",
            Criterion::LessThan => "<",
            Criterion::GreaterEqualTo => ">=",
            Criterion::LessEqualTo => "<=",
            Criterion::NotEqualTo => "!=",
        };

        self.emit(OpCode::LoadCriterion, Some(cr.to_string()));
    }

    fn compile_multi_condition(&mut self, condition: &MultiCondition) {
        self.emit(OpCode::LoadColumn, Some(condition.column.clone()));
        self.compile_criterion(&condition.criterion);
        self.compile_value(&condition.value);
        self.emit(OpCode::Filter, None);
    }

    fn compile_conditions(&mut self, conditions: &[MultiCondition]) {
        for condition in conditions {
            self.compile_multi_condition(condition);
        }
    }

    fn compile_delete_statement(&mut self, stmt: &DeleteStatement) {
        let core = &stmt.core;
        self.emit(OpCode::LoadTable, Some(core.table.clone()));

        if let Some(conditions) = &core.conditions {
            self.compile_conditions(conditions);
        }

        self.emit(OpCode::Delete, None);
    }

    fn compile_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Select(select_statement) => self.compile_select_statement(select_statement),
            Statement::Create(create_statement) => self.compile_create_statement(create_statement),
            Statement::Drop(drop_statement) => self.compile_drop_statement(drop_statement),
            Statement::Insert(insert_statement) => self.compile_insert_statement(insert_statement),
            Statement::Update(update_statement) => self.compile_update_statement(update_statement),
            Statement::Delete(delete_statement) => self.compile_delete_statement(delete_statement),
        }
    }

    fn finish(mut self) -> Vec<Instruction> {
        self.instructions.push(Instruction {
            opcode: OpCode::Halt,
            operand: None,
        });
        self.instructions
    }
}

fn compile(stmt: &Statement) -> Vec<Instruction> {
    let mut compiler = Compiler::new();
    compiler.compile_statement(stmt);
    compiler.finish()
}

// Bytecode Engine
#[derive(Debug, Clone, PartialEq)]
enum StackValue {
    Text(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Table(String),
    Column(String),
    Criterion(String),
}

impl fmt::Display for StackValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StackValue::Text(t) => write!(f, "{t}"),
            StackValue::Int(i) => write!(f, "{i}"),
            StackValue::Float(fl) => write!(f, "{fl}"),
            StackValue::Bool(b) => write!(f, "{b}"),
            StackValue::Table(t) => write!(f, "{t}"),
            StackValue::Column(c) => write!(f, "{c}"),
            StackValue::Criterion(c) => write!(f, "{c}"),
        }
    }
}

#[derive(Debug, Clone)]
struct Row {
    data: HashMap<String, StackValue>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct Table {
    name: String,
    columns: Vec<String>,
    column_types: HashMap<String, String>,
    rows: Vec<Row>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct Database {
    name: String,
    tables: HashMap<String, Table>,
}

#[derive(Debug, Default)]
struct BytecodeEngine {
    stack: Vec<StackValue>,
    databases: HashMap<String, Database>,
    current_db: Option<String>,
    pc: usize,
}

impl BytecodeEngine {
    fn new() -> Self {
        Self::default()
    }

    fn push(&mut self, val: StackValue) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Result<StackValue, String> {
        self.stack
            .pop()
            .ok_or(format!("Stack underflow: pc => {}", self.pc))
    }

    fn peek(&mut self) -> Result<&StackValue, String> {
        self.stack.last().ok_or("Stack empty".to_string())
    }

    fn parse_value(&self, val: &str) -> Result<StackValue, String> {
        if let Some(text) = val.strip_prefix("Text:") {
            Ok(StackValue::Text(text.to_string()))
        } else if let Some(int) = val.strip_prefix("Int:") {
            let int = int
                .parse::<i32>()
                .map_err(|_| format!("Invalid Integer: {int}"))?;
            Ok(StackValue::Int(int))
        } else if let Some(float) = val.strip_prefix("Float:") {
            let float = float
                .parse::<f32>()
                .map_err(|_| format!("Invalid float: {float}"))?;
            Ok(StackValue::Float(float))
        } else if let Some(bool) = val.strip_prefix("Bool:") {
            let bool = bool
                .parse::<bool>()
                .map_err(|_| format!("Invalid bool: {bool}"))?;
            Ok(StackValue::Bool(bool))
        } else {
            Ok(StackValue::Text(val.to_string()))
        }
    }

    fn execute(&mut self, instructions: &[Instruction]) -> Result<Vec<Row>, String> {
        // reset counter
        self.pc = 0;
        let mut result = Vec::new();

        while self.pc < instructions.len() {
            let instruction = &instructions[self.pc];

            match instruction.opcode {
                OpCode::Update => todo!(),
                OpCode::Filter => todo!(),
                OpCode::SetColumn => todo!(),
                OpCode::Halt => break,

                OpCode::Delete => {
                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => return Err(format!("Expected table name, got {other:?}")),
                    };
                    if let Some(db_name) = &self.current_db {
                        if let Some(database) = self.databases.get_mut(db_name) {
                            if let Some(table) = database.tables.get_mut(&table_name) {
                                table.rows.clear(); // Delete all rows (simplified)
                            }
                        }
                    }
                }

                OpCode::Select => {
                    let mut columns_to_select = Vec::new();
                    let mut table_name = String::new();
                    // let mut filter = Vec::new(); // handle this later

                    while let Ok(value) = self.pop() {
                        match value {
                            StackValue::Table(name) => {
                                table_name = name;
                                break;
                            }
                            StackValue::Column(c) => columns_to_select.push(c),
                            _ => {}
                        }
                    }

                    if let Some(db_name) = self.current_db.as_ref() {
                        if let Some(db) = self.databases.get(db_name) {
                            if let Some(table) = db.tables.get(&table_name) {
                                let rows = &table.rows;

                                // Apply filters

                                if columns_to_select.contains(&"*".to_string()) {
                                    result = rows.to_vec();
                                } else {
                                    for row in rows {
                                        let mut new_row_data = HashMap::new();
                                        for col in &columns_to_select {
                                            if let Some(data) = row.data.get(col) {
                                                new_row_data.insert(col.clone(), data.clone());
                                            }
                                        }
                                        result.push(Row { data: new_row_data });
                                    }
                                }
                            }
                        }
                    }
                }
                OpCode::Insert => {
                    let mut columns = Vec::new();
                    let mut values = Vec::new();

                    while let Ok(val) = self.pop() {
                        match val {
                            StackValue::Table(_) => {
                                self.push(val); // push back table
                                break;
                            }
                            StackValue::Column(c) => columns.push(c),
                            other => values.push(other),
                        }
                    }

                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => return Err(format!("Expected table name, got {other:?}")),
                    };

                    columns.reverse();
                    values.reverse();

                    let mut row_data = HashMap::new();

                    for (i, val) in columns.into_iter().enumerate() {
                        if let Some(entry) = values.get(i) {
                            row_data.insert(val.to_string(), entry.clone());
                        }
                    }

                    let row = Row { data: row_data };

                    if let Some(db_name) = self.current_db.as_ref() {
                        if let Some(db) = self.databases.get_mut(db_name) {
                            if let Some(table) = db.tables.get_mut(&table_name) {
                                table.rows.push(row);
                            }
                        }
                    }
                }
                OpCode::DropTable => {
                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => format!("Expected table name, got: {other}"),
                    };

                    if let Some(db_name) = self.current_db.as_ref() {
                        if let Some(db) = self.databases.get_mut(db_name) {
                            db.tables.remove(&table_name);
                        }
                    }
                }
                OpCode::CreateTable => {
                    let mut columns = Vec::new();
                    let mut column_types = HashMap::new();

                    // Collect column definitions from stack
                    while let Ok(value) = self.peek() {
                        match value {
                            StackValue::Column(col) => {
                                let col_name = col.clone();
                                self.pop()?;
                                if let Ok(type_str) = self.peek()
                                    && let StackValue::Text(text) = type_str
                                    && (text.starts_with("Int")
                                        || text.starts_with("Text")
                                        || text.starts_with("Float")
                                        || text.starts_with("Bool"))
                                {
                                    column_types.insert(col_name.clone(), text.to_string());
                                    self.pop()?;
                                }
                            }

                            StackValue::Table(_) => {
                                break;
                            }
                            _ => {
                                self.pop()?;
                                break;
                            }
                        }
                    }

                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => return Err(format!("Expected table name, got {other:?}")),
                    };

                    columns.reverse(); // Stack reverses order

                    let table = Table {
                        name: table_name.clone(),
                        columns,
                        column_types,
                        rows: Vec::new(),
                    };

                    if let Some(db_name) = &self.current_db {
                        if let Some(database) = self.databases.get_mut(db_name) {
                            database.tables.insert(table_name, table);
                        }
                    }
                }
                OpCode::CreateDatabase => {
                    let db_name = match self.pop()? {
                        StackValue::Text(name) => name,
                        other => return Err(format!("Expected database name, got: {other}")),
                    };

                    let db = Database {
                        name: db_name.clone(),
                        tables: HashMap::new(),
                    };

                    self.databases.insert(db_name.clone(), db);
                    self.current_db = Some(db_name);
                }
                OpCode::DropDatabase => {
                    let db_name = match self.pop()? {
                        StackValue::Text(name) => name,
                        other => return Err(format!("Expected database name, got: {other}")),
                    };

                    self.databases.remove(&db_name);
                    if self.current_db.as_ref() == Some(&db_name) {
                        self.current_db = None;
                    }
                }
                OpCode::LoadTable => {
                    let table_name = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadTable needs operand".to_string())?;
                    self.stack.push(StackValue::Table(table_name.clone()));
                }
                OpCode::LoadColumn => {
                    let col_name = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadColumn needs operand".to_string())?;
                    self.stack.push(StackValue::Column(col_name.clone()));
                }
                OpCode::LoadValue => {
                    let val = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadValue needs operand".to_string())?;
                    let value = self.parse_value(val)?;
                    self.stack.push(value);
                }
                OpCode::LoadCriterion => {
                    let criterion = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadCriterion needs operand".to_string())?;
                    self.stack.push(StackValue::Criterion(criterion.clone()));
                }
            }
            self.pc += 1;
        }

        Ok(result)
    }

    fn print_tables(&self) {
        if let Some(db_name) = &self.current_db {
            if let Some(database) = self.databases.get(db_name) {
                println!("Database: {db_name}");
                for (table_name, table) in &database.tables {
                    println!("  Table: {table_name}");
                    println!("    Columns: {:?}", table.columns);
                    println!("    Rows: {}", table.rows.len());
                    for (i, row) in table.rows.iter().enumerate() {
                        println!("      Row {}: {:?}", i, row.data);
                    }
                }
            }
        }
    }
}

fn main() -> Result<(), String> {
    let mut vm = BytecodeEngine::new();
    let stmts = [
        "create database dev;",
        "create table cats(name text);",
        "insert into cats (name) values('foo');",
        "select * from cats;",
    ];

    let mut bytecodes = Vec::with_capacity(4);
    for s in stmts {
        let stmt = parse_statement(s)?;
        // dbg!(&stmt);
        let bytecode = compile(&stmt);
        bytecodes.push(bytecode);
    }

    let mut rows = Vec::new();

    for b in bytecodes {
        println!("{b:#?}");
        let r = vm.execute(&b)?;
        rows.push(r);
    }

    vm.print_tables();
    Ok(())
}

#[cfg(test)]
mod tests {
    mod parse_select {
        use crate::parse_statement;

        #[test]
        fn test_star_statement() {
            let s = "select * from cats;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_multi_columns() {
            let s = "select name, age from cats;";
            assert!(parse_statement(s).is_ok());
        }
        #[test]
        fn test_columns_alias() {
            let s = "select name as n, age as x from cats;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_single_condition() {
            let s = "select * from foo limit 10;";
            assert!(parse_statement(s).is_ok());

            let s = "select * from foo offset 10;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_double_conditions() {
            let s = "SELECT * FROM users LIMIT 10 OFFSET 5;";
            assert!(parse_statement(s).is_ok());

            let s = "SELECT * FROM users ORDER BY created_at DESC;";
            assert!(parse_statement(s).is_ok());

            let s = "SELECT name FROM users ORDER BY age ASC;";
            assert!(parse_statement(s).is_ok());

            let s = "SELECT department FROM employees GROUP BY department;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        #[ignore]
        fn test_multi_condition() {}
    }

    mod parse_drop {
        use crate::parse_statement;

        #[test]
        fn test_database_drop() {
            let s = "drop database cats;";
            assert!(parse_statement(s).is_ok());
        }
        #[test]
        fn test_table_drop() {
            let s = "drop table cats;";
            assert!(parse_statement(s).is_ok());
        }
    }

    mod parse_create {
        use crate::parse_statement;

        #[test]
        fn test_create_database() {
            let s = "create database cats;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_single_row() {
            let s = "create table cats (name text);";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_multi_row() {
            let s = "create table cats (name text, age int);";
            assert!(parse_statement(s).is_ok());
        }
    }

    mod parse_insert {
        use crate::parse_statement;

        #[test]
        fn test_singles() {
            let s = "insert into cats(name) values(mike);";
            assert!(parse_statement(s).is_ok());
            let s = "insert into cats(is_male) values(true);";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_multi_cols() {
            let s = "insert into cats(name, age) values(mike, 10);";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_multi_values() {
            let s = "insert into cats(name, age) values (mike, 10), (stacy, 12);";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_quoted_idents() {
            let s = "insert into cats(name, age) values('mike andrew', 12);";
            assert!(parse_statement(s).is_ok());
        }
    }

    mod parse_update {
        use crate::parse_statement;

        #[test]
        fn test_no_condition() {
            let s = "update cats set age = 25;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_single_column() {
            let s = "update cats set age = 15 where name = 'mike';";
            assert!(parse_statement(s).is_ok());
            let s = "update cats set age = 15 where name > 'mike';";
            assert!(parse_statement(s).is_ok());
            let s = "update cats set age = 15 where name < 'mike';";
            assert!(parse_statement(s).is_ok());
            let s = "update cats set age = 15 where name >= 'mike';";
            assert!(parse_statement(s).is_ok());
            let s = "update cats set age = 15 where name <= 'mike';";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_multi_columns() {
            let s = "update cats set name = 'andrew', age = 20 where name = 'mike';";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_multi_condition() {
            let s = "update cats set name = 'andrew', age = 20 where name = 'mike' and email = 'foo@me.com';";
            assert!(parse_statement(s).is_ok());
            let s = "update cats set name = 'andrew', age = 20 where name = 'mike' or email = 'foo@me.com';";
            assert!(parse_statement(s).is_ok());
        }
    }

    mod parse_delete {
        use crate::parse_statement;

        #[test]
        fn test_no_condition() {
            let s = "delete from cats;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        fn test_single_condition() {
            let s = "delete from cats where name = 20;";
            assert!(parse_statement(s).is_ok());
        }

        #[test]
        #[ignore]
        fn test_multi_conditions() {
            let s = "delete from cats where name = 'mike' and age = 20;";
            assert!(parse_statement(s).is_ok());
        }
    }

    mod bytecode_engine {
        use crate::{BytecodeEngine, Instruction, OpCode};

        #[test]
        fn test_simple() {
            let mut vm = BytecodeEngine::new();

            // Example: CREATE DATABASE test;
            let create_db_instructions = vec![
                Instruction {
                    opcode: OpCode::LoadValue,
                    operand: Some("test".to_string()),
                },
                Instruction {
                    opcode: OpCode::CreateDatabase,
                    operand: None,
                },
                Instruction {
                    opcode: OpCode::Halt,
                    operand: None,
                },
            ];

            let _ = vm.execute(&create_db_instructions).is_ok();

            // Example: CREATE TABLE users (name TEXT, age INT);
            let create_table_instructions = vec![
                Instruction {
                    opcode: OpCode::LoadTable,
                    operand: Some("users".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadColumn,
                    operand: Some("name".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadValue,
                    operand: Some("Text".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadColumn,
                    operand: Some("age".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadValue,
                    operand: Some("Int".to_string()),
                },
                Instruction {
                    opcode: OpCode::CreateTable,
                    operand: None,
                },
                Instruction {
                    opcode: OpCode::Halt,
                    operand: None,
                },
            ];

            let _ = vm.execute(&create_table_instructions).is_ok();

            // Example: INSERT INTO users (name, age) VALUES ('Alice', 25);
            let insert_instructions = vec![
                Instruction {
                    opcode: OpCode::LoadTable,
                    operand: Some("users".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadColumn,
                    operand: Some("name".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadColumn,
                    operand: Some("age".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadValue,
                    operand: Some("Text:Alice".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadValue,
                    operand: Some("Int:25".to_string()),
                },
                Instruction {
                    opcode: OpCode::Insert,
                    operand: None,
                },
                Instruction {
                    opcode: OpCode::Halt,
                    operand: None,
                },
            ];

            let _ = vm.execute(&insert_instructions).is_ok();

            // Example: SELECT * FROM users;
            let select_instructions = vec![
                Instruction {
                    opcode: OpCode::LoadTable,
                    operand: Some("users".to_string()),
                },
                Instruction {
                    opcode: OpCode::LoadColumn,
                    operand: Some("*".to_string()),
                },
                Instruction {
                    opcode: OpCode::Select,
                    operand: None,
                },
                Instruction {
                    opcode: OpCode::Halt,
                    operand: None,
                },
            ];

            let results = vm.execute(&select_instructions).unwrap();

            assert!(!results.is_empty());
        }
    }
}
