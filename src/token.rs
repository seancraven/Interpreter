use anyhow::anyhow;

use crate::ast::OperatorToken;

#[derive(Debug, PartialEq, Clone, Default, Eq, Hash)]
pub enum Token {
    EOF,             // end of file
    Illegal(String), // not allowd tokens
    IDENT(String),   // varible names

    // Keywords
    Let,
    Fn,
    Return,
    If,
    Else,
    True,
    False,

    // Operators
    Equal,
    Not,
    NotEqual,
    Minus,
    Div,
    Mul,
    Larrow,
    Rarrow,

    Intger(usize),

    Assign, // =
    Plus,   // +

    Comma,     // ,
    SemiColon, // ;

    Lparen, // (
    Rparen, // )

    Lbrakcet, // }
    Rbracket, // {
    //
    #[default]
    Default,
}
impl Token {
    pub fn get_int(&self) -> Option<usize> {
        match self {
            Self::Intger(i) => Some(*i),
            _ => None,
        }
    }
    pub fn get_ident(&self) -> Option<&str> {
        match self {
            Self::IDENT(s) => Some(s.as_str()),
            _ => None,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Self::Intger(i) => format!("{}", i),
            Self::IDENT(i) => format!("{}", i),
            _ => todo!(),
        }
    }
}
impl TryInto<OperatorToken> for &Token {
    type Error = anyhow::Error;
    fn try_into(self) -> Result<OperatorToken, Self::Error> {
        Ok(match *self {
            Token::Mul => OperatorToken::Mul,
            Token::Div => OperatorToken::Div,
            Token::Plus => OperatorToken::Plus,
            Token::Minus => OperatorToken::Minus,
            Token::Larrow => OperatorToken::Larrow,
            Token::Rarrow => OperatorToken::Rarrow,
            Token::NotEqual => OperatorToken::NotEqual,
            Token::Equal => OperatorToken::Equal,
            _ => Err(anyhow!("Can't convert {:?} into operator.", self))?,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Precidence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
impl From<&Token> for Precidence {
    fn from(value: &Token) -> Self {
        match *value {
            Token::Equal | Token::NotEqual => Precidence::Equals,
            Token::Rarrow | Token::Larrow => Precidence::LessGreater,
            Token::Plus | Token::Minus => Precidence::Sum,
            Token::Mul | Token::Div => Precidence::Product,
            Token::Lparen => Precidence::Call,
            _ => Precidence::Lowest,
        }
    }
}
mod test {
    use super::*;

    #[test]
    fn test_precidence() {
        assert!(Precidence::Lowest < Precidence::Equals);
        assert!(Precidence::Equals < Precidence::LessGreater);
        assert!(Precidence::LessGreater < Precidence::Sum);
        assert!(Precidence::Sum < Precidence::Product);
        assert!(Precidence::Product < Precidence::Prefix);
        assert!(Precidence::Prefix < Precidence::Call);
    }
    #[test]
    fn test_into_prec() {
        let m: Precidence = (&Token::Mul).into();
        assert_eq!(m, Precidence::Product)
    }
}
