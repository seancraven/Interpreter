use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> Token;
    fn to_string(&self) -> String;
}
#[derive(Debug)]
pub enum Expression {
    None,
    Iden(Identifier),
}
impl Expression {
    fn to_string(&self) -> String {
        todo!()
    }
}
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}
impl Node for Statement {
    fn token_literal(&self) -> Token {
        match self {
            Self::Let(s) => s.token_literal(),
            Self::Return(s) => s.token_literal(),
            Self::Expression(s) => s.token_literal(),
        }
    }
    fn to_string(&self) -> String {
        match self {
            Self::Let(s) => s.to_string(),
            Self::Return(s) => s.to_string(),
            Self::Expression(s) => s.to_string(),
        }
    }
}
#[derive(Debug)]
pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}
impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Expression) -> LetStatement {
        LetStatement { token, name, value }
    }
}
impl Node for LetStatement {
    fn token_literal(&self) -> Token {
        Token::IDENT(self.name.0.clone())
    }
    fn to_string(&self) -> String {
        return format!(
            "let {} = {}",
            &self.name.to_string(),
            self.value.to_string(),
        );
    }
}
#[derive(Debug)]
pub struct Identifier(pub String);

impl Node for Identifier {
    fn token_literal(&self) -> Token {
        Token::IDENT(self.0.clone())
    }
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    token: Token,
    expression: Expression,
}
impl ReturnStatement {
    pub fn new(token: Token, expression: Expression) -> ReturnStatement {
        ReturnStatement { expression, token }
    }
}
impl Node for ReturnStatement {
    fn token_literal(&self) -> Token {
        Token::Return
    }
    fn to_string(&self) -> String {
        format!("return {};", self.expression.to_string())
    }
}
#[derive(Debug)]
pub struct ExpressionStatement {
    token: Token,
    expression: Expression,
}
impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> ExpressionStatement {
        ExpressionStatement { token, expression }
    }
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }
    fn to_string(&self) -> String {
        format!("{}", self.expression.to_string())
    }
}
