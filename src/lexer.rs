use std::{char, str::Chars};

use crate::token::Token;
pub struct Lexer {
    input: String,
}
impl Lexer {
    fn new(input: impl Into<String>) -> Lexer {
        let input = input.into();
        Lexer { input }
    }
}
impl<'s> IntoIterator for &'s Lexer {
    type Item = Token;
    type IntoIter = <LexerIterator<'s> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        let mut chars = self.input.chars();
        let current_char = chars.next();
        LexerIterator {
            chars,
            current_char,
            end: false,
        }
    }
}
pub struct LexerIterator<'s> {
    chars: Chars<'s>,
    current_char: Option<char>,
    end: bool,
}
impl<'s> Iterator for LexerIterator<'s> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        };
        self.skip_whitespace();
        let Some(n) = self.current_char else {
            self.end = true;
            return Some(Token::EOF);
        };
        // primed flag indicates if the next token for parsing is self.current_char.
        let mut primed = false;
        let out = Some(match n {
            '+' => Token::Plus,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrakcet,
            '}' => Token::Rbracket,
            '/' => Token::Div,
            '*' => Token::Mul,
            '<' => Token::Larrow,
            '>' => Token::Rarrow,
            '-' => Token::Minus,
            '0'..='9' => {
                primed = true;
                self.read_num()
            }
            '=' | '!' => {
                primed = true;
                self.read_operand()
            }
            _ => {
                primed = true;
                self.read_string()
            }
        });
        if !primed {
            self.current_char = self.chars.next();
        }
        out
    }
}
impl<'s> LexerIterator<'s> {
    // skip the whitespace and prime the next token for parsing.
    fn skip_whitespace(&mut self) {
        loop {
            match self.current_char {
                Some('\n') | Some('\t') | Some(' ') | Some('\r') => {
                    self.current_char = self.chars.next();
                }
                _ => {
                    return;
                }
            };
        }
    }

    // Read string and prime next token for parsing.
    fn read_string(&mut self) -> Token {
        let mut string_to_match = String::from("");
        loop {
            if self.current_char.is_none() {
                break;
            }
            match self.current_char.unwrap() {
                'a'..='z' | 'A'..='Z' | '_' => {
                    string_to_match.push(self.current_char.unwrap());
                    self.current_char = self.chars.next();
                }
                _ => {
                    break;
                }
            }
        }
        match &*string_to_match {
            "let" => Token::Let,
            "fn" => Token::Fn,
            "return" => Token::Return,
            "if" => Token::If,
            "true" => Token::True,
            "false" => Token::False,
            "else" => Token::Else,
            _ => Token::IDENT(string_to_match),
        }
    }
    // Read number and prime next token for parsing.
    fn read_num(&mut self) -> Token {
        let mut string_to_parse = String::from("");
        loop {
            if self.current_char.is_none() {
                break;
            }
            match self.current_char.unwrap() {
                '0'..='9' => {
                    string_to_parse.push(self.current_char.unwrap());
                    self.current_char = self.chars.next();
                }
                _ => {
                    break;
                }
            }
        }
        let int_: isize = string_to_parse
            .parse()
            .expect(&*format!("Couldn't parse {} to int", string_to_parse));
        return Token::Intger(int_);
    }
    fn read_operand(&mut self) -> Token {
        let mut string_to_check = String::from("");
        loop {
            if self.current_char.is_none() {
                break;
            }
            match self.current_char.unwrap() {
                '=' | '!' => {
                    string_to_check.push(self.current_char.unwrap());
                    self.current_char = self.chars.next();
                }
                _ => {
                    break;
                }
            }
        }
        match &*string_to_check {
            "==" => Token::Equal,
            "!=" => Token::NotEqual,
            "=" => Token::Assign,
            "!" => Token::Not,
            _ => Token::Illegal(string_to_check),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::Lexer;

    fn test(input: &str, tests: Vec<Token>) {
        let lexer = Lexer::new(input);
        for (token, lexed_token) in tests.iter().zip(lexer.into_iter()) {
            println!("lt {:?}", lexed_token);
            assert_eq!(lexed_token, *token)
        }
    }

    #[test]
    fn test_next_token() {
        let input = "+={}();,";
        let tests: Vec<Token> = vec![
            Token::Plus,
            Token::Assign,
            Token::Lbrakcet,
            Token::Rbracket,
            Token::Lparen,
            Token::Rparen,
            Token::SemiColon,
            Token::Comma,
            Token::EOF,
        ];
        test(input, tests);
    }
    #[test]
    fn test_skip_whitespace() {
        let input = "let x = 5;\n";
        let tests = vec![
            Token::Let,
            Token::IDENT(String::from("x")),
            Token::Assign,
            Token::Intger(5),
            Token::SemiColon,
            Token::EOF,
        ];
        test(input, tests);
    }
    #[test]
    fn test_operands() {
        let input = "let x <>!=;";
        let tests = vec![
            Token::Let,
            Token::IDENT(String::from("x")),
            Token::Larrow,
            Token::Rarrow,
            Token::NotEqual,
            Token::SemiColon,
        ];
        test(input, tests)
    }
    #[test]
    fn test_keywords() {
        let input = "let return fn";
        let tests = vec![Token::Let, Token::Return, Token::Fn];
        test(input, tests);
    }
    #[test]
    fn test_nesting() {
        let input = "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);

        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        };";
        let tests = vec![
            Token::Let,
            Token::IDENT(String::from("five")),
            Token::Assign,
            Token::Intger(5),
            Token::SemiColon,
            Token::Let,
            Token::IDENT(String::from("ten")),
            Token::Assign,
            Token::Intger(10),
            Token::SemiColon,
            Token::Let,
            Token::IDENT(String::from("add")),
            Token::Assign,
            Token::Fn,
            Token::Lparen,
            Token::IDENT(String::from("x")),
            Token::Comma,
            Token::IDENT(String::from("y")),
            Token::Rparen,
            Token::Lbrakcet,
            Token::IDENT(String::from("x")),
            Token::Plus,
            Token::IDENT(String::from("y")),
            Token::SemiColon,
            Token::Rbracket,
            Token::SemiColon,
            Token::Let,
            Token::IDENT(String::from("result")),
            Token::Assign,
            Token::IDENT(String::from("add")),
            Token::Lparen,
            Token::IDENT(String::from("five")),
            Token::Comma,
            Token::IDENT(String::from("ten")),
            Token::Rparen,
            Token::SemiColon,
            Token::Not,
            Token::Minus,
            Token::Div,
            Token::Mul,
            Token::Intger(5),
            Token::SemiColon,
            Token::Intger(5),
            Token::Larrow,
            Token::Intger(10),
            Token::Rarrow,
            Token::Intger(5),
            Token::SemiColon,
            Token::If,
            Token::Lparen,
            Token::Intger(5),
            Token::Larrow,
            Token::Intger(10),
            Token::Rparen,
            Token::Lbrakcet,
            Token::Return,
            Token::True,
            Token::SemiColon,
            Token::Rbracket,
            Token::Else,
            Token::Lbrakcet,
            Token::Return,
            Token::False,
            Token::SemiColon,
            Token::Rbracket,
            Token::SemiColon,
            Token::EOF,
        ];
        test(input, tests)
    }
}
