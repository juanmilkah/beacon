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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
enum ColumnExpr {
    Name(ColumnName),
    Typed(TypedColumn),
    Assignment(ValueAssignment),
}

#[derive(Debug, PartialEq)]
struct ValueAssignment {
    name: String,
    value: Value,
}

#[derive(Debug, PartialEq)]
struct ColumnName {
    name: String,
}

#[derive(Debug, PartialEq)]
struct TypedColumn {
    col_name: String,
    data_type: DataType,
}

#[derive(Debug, PartialEq)]
enum DataType {
    Int,
    Float,
    Text,
    Bool,
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

fn parse_statement(input: &str) -> Result<Statement, String> {
    let tokens = tokenize(input)?;
    dbg!(&tokens);
    let mut parser = Parser::new(tokens);
    let stmt = parser.parse_statement()?;
    parser.expect_eq(Token::SemiColon)?;
    Ok(stmt)
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
fn main() -> Result<(), String> {
    // let s = "select names as n from cats;";
    // let s = "drop table cats;";
    // let s = "create table cats (name text, age int) ;";
    // let s = "insert into cats(name, age) values(mike, 10);";
    // let s = "insert into cats(name, age) values('mike andrew', 12);";
    // let s = "update cats set count = 0 where age = 10;";
    // let s = "update cats set name = 'andrew', age = 20 where name = 'mike';";
    // let s = "insert into cats(is_male) values(true);";
    // let s = "select * from foo offset 10;";
    // let s = "SELECT * FROM users LIMIT 10 OFFSET 5;";
    // let s = "SELECT * FROM users ORDER BY created_at DESC;";
    let s = "SELECT department FROM employees GROUP BY department;";
    let stmt = parse_statement(s)?;
    println!("{stmt:#?}");

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
}
