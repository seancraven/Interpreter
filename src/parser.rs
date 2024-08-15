use std::rc::Rc;

use anyhow::anyhow;

use crate::lexer::{Lexer, LexerIterator};
use crate::token::Token;
trait Node {
    fn token_literal(&self) -> Token;
}

#[derive(Debug)]
enum Expression {
    None,
}
#[derive(Debug)]
enum Statement {
    Let(LetStatement),
}
impl Node for Statement {
    fn token_literal(&self) -> Token {
        match self {
            Self::Let(s) => s.token_literal(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<anyhow::Error>,
}
#[derive(Debug)]
struct Identifier(String);
impl Node for Identifier {
    fn token_literal(&self) -> Token {
        Token::IDENT(self.0.clone())
    }
}

#[derive(Debug)]
struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}
impl LetStatement {
    fn new(token: Token, name: Identifier, value: Expression) -> LetStatement {
        LetStatement { token, name, value }
    }
}
impl Node for LetStatement {
    fn token_literal(&self) -> Token {
        Token::IDENT(self.name.0.clone())
    }
}

struct Parser<'s> {
    l: LexerIterator<'s>,
    current_token: Token,
    next_token: Token,
}
impl<'s> Parser<'s> {
    fn new(lexer: &'s Lexer) -> Parser<'s> {
        let mut l = lexer.into_iter();
        let current_token = l.next().unwrap();
        let next_token = l.next().unwrap();
        Parser {
            l,
            current_token,
            next_token,
        }
    }
    fn parse_expression(&mut self) -> Option<Expression> {
        todo!()
    }
    fn parse_statement(&mut self) -> anyhow::Result<Option<Statement>> {
        match self.current_token {
            Token::Let => {
                let token = std::mem::take(&mut self.current_token);
                let name = match self.next_token.clone() {
                    Token::IDENT(i) => Identifier(i),
                    _ => {
                        return Err(anyhow!(
                            "Let must be followed by identifier. Got {:?}",
                            self.current_token
                        ))
                    }
                };
                self.next();
                if self.next_token != Token::Assign {
                    return Err(anyhow!("Identifier must be followed by assign."));
                }
                self.next();
                self.next();
                // let value = self.parse_expression().expect("Couldn't parse expression.");
                let out = Ok(Some(Statement::Let(LetStatement::new(
                    token,
                    name,
                    Expression::None,
                ))));
                while let Some(t) = self.next() {
                    if *t == Token::SemiColon {
                        break;
                    }
                }
                out
            }
            _ => return Ok(None),
        }
    }
    fn next(&mut self) -> Option<&Token> {
        self.current_token = std::mem::take(&mut self.next_token);
        match self.l.next() {
            Some(t) => {
                self.next_token = t;
                Some(&self.current_token)
            }
            None => {
                self.next_token = Token::EOF;
                None
            }
        }
    }
    fn parse_program(&mut self) -> anyhow::Result<Program> {
        let mut statements = vec![];
        let mut errors = vec![];
        while self.current_token != Token::EOF {
            match self.parse_statement() {
                Ok(None) => (),
                Ok(Some(s)) => statements.push(s),
                Err(e) => {
                    eprintln!("{}", e);
                    errors.push(e);
                }
            };
            self.next();
        }
        Ok(Program { statements, errors })
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn test_let_statement() {
        let input = "let x = 5;\nlet y = 10;\nlet boobies = 500535;";
        let lexer = Lexer::new(input);
        let p = Parser::new(&lexer).parse_program().unwrap();
        assert_eq!(p.statements.len(), 3, "{:?}", p.statements);
        let ident_tests = vec!["x", "y", "boobies"];
        for (expected_ident, stmt) in ident_tests.iter().zip(p.statements) {
            assert_eq!(
                stmt.token_literal(),
                Token::IDENT(String::from(*expected_ident))
            )
        }
    }
    #[test]
    fn test_error_parsing() {
        let input = "let x;\nlet x 5;\nlet = 5;";
        let lexer = Lexer::new(input);
        let p = Parser::new(&lexer).parse_program().unwrap();
        assert_eq!(p.errors.len(), 3, "{:?}", p.errors)
    }
}
