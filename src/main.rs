#[derive(Debug, Eq, PartialEq)]
enum Token {
    As,
    Comma,
    Create,
    Database,
    Drop,
    From,
    Identifier(String),
    Int,
    LeftParen,
    RightParen,
    Select,
    SemiColon,
    Star,
    Table,
    Text,
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
        matches!(self, Token::Int | Token::Text)
    }

    fn to_data_type(&self) -> Result<DataType, String> {
        match self {
            Token::Int => Ok(DataType::Int),
            Token::Text => Ok(DataType::Text),
            other => Err(format!("Unknown data type: {other:?}")),
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
            c if c.is_whitespace() => continue,
            c if c.is_alphabetic() => {
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
    }

    /// Get string representation of ident; fail if token is not ident
    fn expect_identifier(&mut self) -> Result<&str, String> {
        self.expect_matching(|t| matches!(t, Token::Identifier(_)))
            .map(|t| t.as_identifier().unwrap())
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

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.tokens.first() {
            Some(token) => match token {
                Token::Drop => Ok(Statement::Drop(self.parse_drop()?)),
                Token::Select => Ok(Statement::Select(self.parse_select()?)),
                Token::Create => Ok(Statement::Create(self.parse_create()?)),
                other => Err(format!("Unexpected command token: {other:?}")),
            },
            None => Err("Unexpected end on input".to_string()),
        }
    }
}

fn parse_statement(input: &str) -> Result<Statement, String> {
    let tokens = tokenize(input)?;
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
fn main() -> Result<(), String> {
    // let stmt = "select names as n from cats;";
    // let stmt = "drop table cats;";
    let s = "create table cats (name text, age int) ;";
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
}
