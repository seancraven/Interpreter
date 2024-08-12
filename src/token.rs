#[derive(Debug, PartialEq)]
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
}
