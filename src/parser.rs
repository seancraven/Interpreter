use std::collections::HashMap;

use anyhow::anyhow;

use crate::ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, ReturnStatement, Statement,
};
use crate::lexer::{Lexer, LexerIterator};
use crate::token::{Precidence, Token, TokenMap};

pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<anyhow::Error>,
}
pub struct Parser<'s> {
    l: LexerIterator<'s>,
    current_token: Token,
    next_token: Token,
    tm: TokenMap,
}
impl<'s> Parser<'s> {
    pub fn new(lexer: &'s Lexer) -> Parser<'s> {
        let mut l = lexer.into_iter();
        let current_token = l.next().unwrap();
        let next_token = l.next().unwrap();
        let mut tm = TokenMap::new();
        tm.insert_prefix(&Token::IDENT(String::new()), Box::new(Parser::parse_ident));
        Parser {
            l,
            current_token,
            next_token,
            tm,
        }
    }
    fn parse_ident(t: &Token) -> Expression {
        match t.clone() {
            Token::IDENT(s) => Expression::Iden(Identifier(s.clone())),
            _ => panic!("Tried parsing identifier, but the current token isn't an identifier."),
        }
    }
    fn parse_expression(&mut self, p: Precidence) -> Option<Expression> {
        let Some(prefix) = self.tm.get_prefix(&self.current_token) else {
            return None;
        };
        let left_exp = prefix(&self.current_token);
        return Some(left_exp);
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
                // let value = self
                //     .parse_expression(Precidence::Lowest)
                //     .expect("Couldn't parse expression.");
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
            Token::Return => {
                let token = std::mem::take(&mut self.current_token);
                let exp = Expression::None;
                let out: anyhow::Result<Option<Statement>> =
                    Ok(Some(Statement::Return(ReturnStatement::new(token, exp))));
                while let Some(t) = self.next() {
                    if *t == Token::SemiColon {
                        break;
                    }
                }
                out
            }
            _ => {
                let Some(exp) = self.parse_expression(Precidence::Lowest) else {
                    return Ok(None);
                };
                let stmt_token = std::mem::take(&mut self.current_token);
                let stmt = ExpressionStatement::new(stmt_token, exp);
                return Ok(Some(Statement::Expression(stmt)));
            }
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
    pub fn parse_program(&mut self) -> anyhow::Result<Program> {
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
    use core::panic;

    use crate::lexer::Lexer;

    use super::*;
    use crate::ast::Node;

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
    #[test]
    fn test_return_statement() {
        let input = "return 5;\nreturn 15;\nreturn x;";
        let lexer = Lexer::new(input);
        let p = Parser::new(&lexer).parse_program().unwrap();
        assert_eq!(p.statements.len(), 3);
        for stmt in p.statements {
            match stmt {
                Statement::Return(_) => (),
                _ => panic!("Wrong statement type."),
            }
        }
    }
    #[test]
    fn test_expression_statement() {
        let input = "fubar;";
        let lexer = Lexer::new(input);
        let p = Parser::new(&lexer).parse_program().unwrap();
        assert_eq!(p.statements.len(), 1, "Expected one expression statement.");
        match p.statements[0] {
            Statement::Expression(_) => (),
            _ => panic!("Wrong statement type."),
        }
    }
}
