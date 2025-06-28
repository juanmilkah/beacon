#[derive(Debug, Eq, PartialEq)]
enum Token {
    As,
    Comma,
    From,
    Select,
    Identifier(String),
    SemiColon,
    Star,
}

impl Token {
    /// Return string representation of token if is an identifier
    fn as_identifier(&self) -> Option<&str> {
        match self {
            Token::Identifier(s) => Some(s),
            _ => None,
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
}

#[derive(Debug, Eq, PartialEq)]
struct Column {
    name: String,
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
        Ok(Expr::Column(Column {
            name: self.expect_identifier()?.to_string(),
        }))
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

    fn parse_statement(&mut self) -> Result<Statement, String> {
        Ok(Statement::Select(self.parse_select()?))
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
fn main() -> Result<(), String> {
    let stmt = "select names as n froom cats;";
    let stmt = parse_statement(stmt)?;
    println!("{stmt:#?}");

    Ok(())
}
