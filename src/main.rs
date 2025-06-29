#[derive(Debug, Eq, PartialEq)]
enum Token {
    And,
    As,
    Bang,
    BangEqual,
    Comma,
    Create,
    Database,
    Drop,
    Equal,
    Float,
    From,
    Greater,
    GreaterEqual,
    Identifier(String),
    Insert,
    Int,
    Into,
    LeftParen,
    Less,
    LessEqual,
    Or,
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
            Token::Identifier(s) => Some(s),
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
            other => Err(format!("Unknown data type: {other:?}")),
        }
    }

    fn to_value(&self) -> Result<Value, String> {
        match self {
            Token::Identifier(ident) => match ident.parse::<i32>() {
                Ok(num) => Ok(Value::Int(num)),
                Err(_) => Ok(Value::Text(ident.to_string())),
            },
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
                tokens.push(Token::Identifier(ident))
            }
            c if c.is_whitespace() => continue,
            c if c.is_alphanumeric() => {
                let mut ident = c.to_string().to_lowercase();
                while let Some(cc) = input.next_if(|cc| cc.is_alphanumeric() || *cc == '_') {
                    ident.extend(cc.to_lowercase());
                }

                match ident.as_str() {
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
                    "into" => tokens.push(Token::Into),
                    "insert" => tokens.push(Token::Insert),
                    "values" => tokens.push(Token::Values),
                    "update" => tokens.push(Token::Update),
                    "where" => tokens.push(Token::Where),
                    "set" => tokens.push(Token::Set),
                    "and" => tokens.push(Token::And),
                    "or" => tokens.push(Token::Or),
                    _ => tokens.push(Token::Identifier(ident)),
                }
            }

            _ => return Err(format!("Unexpected character: {c}")),
        }
    }

    Ok(tokens)
}

/// Ast Representation
#[derive(Debug, Eq, PartialEq)]
enum Statement {
    Select(SelectStatement),
    Create(CreateStatement),
    Drop(DropStatement),
    Insert(InsertStatement),
    Update(UpdateStatement),
}

#[derive(Debug, Eq, PartialEq)]
struct UpdateStatement {
    core: UpdateCore,
}

#[derive(Debug, Eq, PartialEq)]
struct UpdateCore {
    table: String,
    columns: Vec<Expr>,
    conditions: Option<Vec<Condition>>,
}

#[derive(Debug, Eq, PartialEq)]
struct Condition {
    column: String,
    criterion: Criterion,
    value: Value,
}

#[derive(Debug, Eq, PartialEq)]
enum Value {
    Text(String),
    Int(i32),
    // Float(f32), //TODO! handle floats
}

#[derive(Debug, Eq, PartialEq)]
enum Criterion {
    GreaterThan,
    EqualTo,
    LessThan,
    GreaterEqualTo,
    LessEqualTo,
    NotEqualTo,
}

#[derive(Debug, Eq, PartialEq)]
struct InsertStatement {
    core: InsertCore,
}

#[derive(Debug, Eq, PartialEq)]
struct InsertCore {
    table: String,
    columns: Vec<Expr>,
    values: Vec<Vec<Expr>>,
}

#[derive(Debug, Eq, PartialEq)]
struct CreateStatement {
    core: CreateCore,
}

#[derive(Debug, Eq, PartialEq)]
struct CreateCore {
    family: CreateFamily,
}

#[derive(Debug, Eq, PartialEq)]
enum CreateFamily {
    Database(CreateDatabase),
    Table(CreateTable),
}

#[derive(Debug, Eq, PartialEq)]
struct CreateTable {
    name: String,
    columns: Vec<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
struct CreateDatabase {
    name: String,
}

#[derive(Debug, Eq, PartialEq)]
struct DropStatement {
    core: DropCore,
}

#[derive(Debug, Eq, PartialEq)]
struct DropCore {
    family: DropFamily,
}

#[derive(Debug, Eq, PartialEq)]
enum DropFamily {
    Database(DropDatabase),
    Table(DropTable),
}

#[derive(Debug, Eq, PartialEq)]
struct DropTable {
    name: String,
}

#[derive(Debug, Eq, PartialEq)]
struct DropDatabase {
    name: String,
}

#[derive(Debug, Eq, PartialEq)]
struct SelectStatement {
    core: SelectCore,
}

#[derive(Debug, Eq, PartialEq)]
struct SelectCore {
    result_columns: Vec<ResultColumn>,
    from: SelectFrom,
}

#[derive(Debug, Eq, PartialEq)]
enum SelectFrom {
    Table(String),
}

#[derive(Debug, Eq, PartialEq)]
enum ResultColumn {
    Star,
    Expr(ExprResultColumn),
}

#[derive(Debug, Eq, PartialEq)]
struct ExprResultColumn {
    expr: Expr,
    alias: Option<String>,
}

#[derive(Debug, Eq, PartialEq)]
enum Expr {
    Column(Column),
    FatColumn(FatColumn),
    PregnantColumn(PregnantColumn),
}

#[derive(Debug, Eq, PartialEq)]
struct PregnantColumn {
    name: String,
    value: Value,
}

#[derive(Debug, Eq, PartialEq)]
struct Column {
    name: String,
}

#[derive(Debug, Eq, PartialEq)]
struct FatColumn {
    col_name: String,
    data_type: DataType,
}

#[derive(Debug, Eq, PartialEq)]
enum DataType {
    Int,
    Float,
    Text,
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
        self.expect_matching(|t| matches!(t, Token::Identifier(_)))
            .map(|t| t.as_identifier().unwrap())
    }

    fn expect_value(&mut self) -> Result<Value, String> {
        self.expect_matching(|t| matches!(t, Token::Identifier(_)))
            .map(|t| t.to_value())?
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let name = self.expect_identifier()?.to_string();
        match self.peek_next_token() {
            None => Err("Unexpected end of input".to_string()),
            Some(token) => {
                if token.is_data_type() {
                    let data_type = token.to_data_type().unwrap();
                    self.advance();
                    Ok(Expr::FatColumn(FatColumn {
                        col_name: name,
                        data_type,
                    }))
                } else {
                    Ok(Expr::Column(Column { name }))
                }
            }
        }
    }

    fn parse_expr_result_column(&mut self) -> Result<ExprResultColumn, String> {
        let expr = self.parse_expr()?;
        let alias = if self.next_token_is(Token::As) {
            self.advance();
            Some(self.expect_identifier()?.to_string())
        } else {
            None
        };

        Ok(ExprResultColumn { expr, alias })
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

    fn parse_select(&mut self) -> Result<SelectStatement, String> {
        self.expect_eq(Token::Select)?;
        let result_columns = self.parse_result_columns()?;
        self.expect_eq(Token::From)?;
        let from = self.parse_select_from()?;

        Ok(SelectStatement {
            core: SelectCore {
                result_columns,
                from,
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

    fn parse_create_table_columns(&mut self) -> Result<Vec<Expr>, String> {
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

    fn parse_insert_values(&mut self) -> Result<Vec<Vec<Expr>>, String> {
        self.expect_eq(Token::LeftParen)?;
        let mut vals = vec![self.parse_insert_columns()?];
        self.expect_eq(Token::RightParen)?;

        while self.next_token_is(Token::Comma) {
            self.advance();
            self.expect_eq(Token::LeftParen)?;
            vals.push(self.parse_insert_columns()?);
            self.expect_eq(Token::RightParen)?;
        }
        Ok(vals)
    }

    fn parse_insert_columns(&mut self) -> Result<Vec<Expr>, String> {
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

    fn parse_update_condition(&mut self) -> Result<Condition, String> {
        let column = self.expect_identifier()?.to_string();
        let criterion = self.expect_criterion()?;
        let value = self.expect_value()?;

        Ok(Condition {
            column,
            criterion,
            value,
        })
    }

    fn parse_update_conditions(&mut self) -> Result<Option<Vec<Condition>>, String> {
        if !matches!(self.next_token(), Some(Token::Where)) {
            return Ok(None);
        }

        let mut conds = Vec::new();
        // TODO! What's the guard for condition?
        loop {
            conds.push(self.parse_update_condition()?);
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

    fn parse_update_column(&mut self) -> Result<Expr, String> {
        let name = self.expect_identifier()?.to_string();
        self.expect_eq(Token::Equal)?;
        let value = self.expect_value()?;

        Ok(Expr::PregnantColumn(PregnantColumn { name, value }))
    }

    fn parse_update_columns(&mut self) -> Result<Vec<Expr>, String> {
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
        let conditions = self.parse_update_conditions()?;
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

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.tokens.first() {
            Some(token) => match token {
                Token::Drop => Ok(Statement::Drop(self.parse_drop()?)),
                Token::Select => Ok(Statement::Select(self.parse_select()?)),
                Token::Create => Ok(Statement::Create(self.parse_create()?)),
                Token::Insert => Ok(Statement::Insert(self.parse_insert()?)),
                Token::Update => Ok(Statement::Update(self.parse_update()?)),
                other => Err(format!("Unexpected command token: {other:?}")),
            },
            None => Err("Unexpected end on input".to_string()),
        }
    }
}

fn parse_statement(input: &str) -> Result<Statement, String> {
    let tokens = tokenize(input)?;
    // dbg!(&tokens);
    let mut parser = Parser::new(tokens);
    let stmt = parser.parse_statement()?;
    parser.expect_eq(Token::SemiColon)?;
    Ok(stmt)
}

// select * from foo;
// select name from foo;
// select name as n from foo;
// select name, age as x from foo;

// drop table foo;
// drop database foo;

// create database foo;
// create table foo(name text);
// create table foo(name text, age int);

// insert into cats(name) values(tracy);
// insert into cats(name, age) values(tracy, 10);
// insert into cats(name, age) values (tracy, 10), (mike, 12);
// insert into cats(name, age) values('mike andrew', 12);

// update cats set age = 15 where name = 'mike';
// update cats set name = 'andrew', age = 20 where name = 'mike';
fn main() -> Result<(), String> {
    // let stmt = "select names as n from cats;";
    // let stmt = "drop table cats;";
    // let s = "create table cats (name text, age int) ;";
    // let s = "insert into cats(name, age) values(mike, 10);";
    // let s = "insert into cats(name, age) values('mike andrew', 12);";
    // let s = "update cats set count = 0 where age = 10;";
    let s = "update cats set name = 'andrew', age = 20 where name = 'mike';";
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
}
