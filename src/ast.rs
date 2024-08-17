use std::{fmt::format, usize};

use anyhow::anyhow;

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> Token;
    fn to_string(&self) -> String;
}
#[derive(Debug, PartialEq, Clone)]
pub enum PrefixToken {
    Not,
    Minus,
}
impl TryFrom<Token> for PrefixToken {
    type Error = anyhow::Error;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Minus => Ok(PrefixToken::Minus),
            Token::Not => Ok(PrefixToken::Not),
            _ => Err(anyhow!("Invalid conversion must be !, -")),
        }
    }
}
impl PrefixToken {
    pub fn to_string(&self) -> String {
        String::from(match self {
            Self::Not => "!",
            Self::Minus => "-",
        })
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum OperatorToken {
    Plus,
    Minus,
    Equal,
    NotEqual,
    Larrow,
    Rarrow,
    Mul,
    Div,
}
impl OperatorToken {
    pub fn to_string(&self) -> String {
        String::from(match self {
            Self::Minus => "-",
            Self::Plus => "+",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Rarrow => ">",
            Self::Larrow => "<",
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    None,
    Prefix {
        token: PrefixToken,
        right: Box<Expression>,
    },
    Iden(String),
    Int(usize),
    Infix {
        operator_token: OperatorToken,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}
impl Expression {
    pub fn to_string(&self) -> String {
        match self {
            Expression::Prefix { token, right } => {
                format!("({}{})", token.to_string(), right.to_string())
            }
            Expression::Infix {
                operator_token,
                left,
                right,
            } => format!(
                "({} {} {})",
                left.to_string(),
                operator_token.to_string(),
                right.to_string()
            ),
            Expression::Int(i) => format!("{}", i),
            Expression::Iden(i) => format!("{}", i),
            _ => todo!(),
        }
    }
    pub fn get_int(&self) -> Option<usize> {
        match self {
            Self::Int(i) => Some(*i),
            _ => None,
        }
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
impl Statement {
    pub fn get_expression(&self) -> Option<&ExpressionStatement> {
        match self {
            Self::Expression(a) => Some(a),
            _ => None,
        }
    }
}
#[derive(Debug)]
pub struct LetStatement {
    name: Identifier,
    value: Expression,
}
impl LetStatement {
    pub fn new(name: Identifier, value: Expression) -> LetStatement {
        LetStatement { name, value }
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
#[derive(Debug, PartialEq)]
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
    expression: Expression,
}
impl ReturnStatement {
    pub fn new(expression: Expression) -> ReturnStatement {
        ReturnStatement { expression }
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
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    token: Token,
    pub expression: Expression,
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
