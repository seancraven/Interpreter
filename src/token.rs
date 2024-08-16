use std::collections::HashMap;

use crate::ast::Expression;
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

    Intger(isize),

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

type PrefixParseFn = Box<dyn Fn(&Token) -> Expression>;
type InfixParseFn = Box<dyn Fn(&Token, &Expression) -> Expression>;

pub struct TokenMap {
    prefix_fns: HashMap<usize, PrefixParseFn>,
    infix_fns: HashMap<usize, InfixParseFn>,
}
impl TokenMap {
    pub fn new() -> TokenMap {
        TokenMap {
            prefix_fns: HashMap::with_capacity(28),
            infix_fns: HashMap::with_capacity(28),
        }
    }
    pub fn insert_infix(&mut self, token: &Token, fn_: InfixParseFn) -> Option<InfixParseFn> {
        self.infix_fns.insert(TokenMap::token_index(&token), fn_)
    }
    pub fn insert_prefix(&mut self, token: &Token, fn_: PrefixParseFn) -> Option<PrefixParseFn> {
        self.prefix_fns.insert(TokenMap::token_index(&token), fn_)
    }
    pub fn get_prefix(&mut self, token: &Token) -> Option<&PrefixParseFn> {
        self.prefix_fns.get(&TokenMap::token_index(token))
    }

    pub fn get_infix(&mut self, token: &Token) -> Option<&InfixParseFn> {
        self.infix_fns.get(&TokenMap::token_index(token))
    }
    fn token_index(token: &Token) -> usize {
        match token {
            Token::EOF => 0,
            Token::Illegal(_) => 1,
            Token::IDENT(_) => 2,
            Token::Let => 4,
            Token::Fn => 5,
            Token::Return => 6,
            Token::If => 7,
            Token::Else => 8,
            Token::True => 9,
            Token::False => 10,
            Token::Equal => 11,
            Token::Not => 12,
            Token::NotEqual => 13,
            Token::Minus => 14,
            Token::Div => 15,
            Token::Mul => 16,
            Token::Larrow => 17,
            Token::Rarrow => 18,
            Token::Intger(_) => 19,
            Token::Assign => 20,
            Token::Plus => 21,
            Token::Comma => 22,
            Token::SemiColon => 23,
            Token::Lparen => 24,
            Token::Rparen => 25,
            Token::Lbrakcet => 26,
            Token::Rbracket => 27,
            Token::Default => 28,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Precidence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
mod test {
    use crate::token::Precidence;

    #[test]
    fn test_precidence() {
        assert!(Precidence::Lowest < Precidence::Equals);
        assert!(Precidence::Equals < Precidence::LessGreater);
        assert!(Precidence::LessGreater < Precidence::Sum);
        assert!(Precidence::Sum < Precidence::Product);
        assert!(Precidence::Product < Precidence::Prefix);
        assert!(Precidence::Prefix < Precidence::Call);
    }
}
