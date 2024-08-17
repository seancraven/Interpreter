use anyhow::{anyhow, Context};

use crate::ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, PrefixToken, ReturnStatement,
    Statement,
};
use crate::lexer::{Lexer, LexerIterator};
use crate::token::{Precidence, Token};

pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<anyhow::Error>,
}
pub struct Parser<'s> {
    l: LexerIterator<'s>,
    current_token: Token,
    next_token: Token,
}
impl<'s> Parser<'s> {
    pub fn new(lexer: &'s Lexer) -> Parser<'s> {
        let mut l = lexer.into_iter();
        let current_token = l.next().unwrap();
        let next_token = l.next().unwrap();
        Parser {
            l,
            current_token,
            next_token,
        }
    }
    fn parse_infix(&mut self, left: Box<Expression>) -> anyhow::Result<Expression> {
        let prec = self.current_token_precidence();
        let operator_token = (&self.current_token).try_into()?;
        self.next();
        let right = Box::new(self.parse_expression(prec)?);

        Ok(Expression::Infix {
            operator_token,
            left,
            right,
        })
    }
    fn parse_prefix(&mut self) -> anyhow::Result<Expression> {
        match &self.current_token {
            Token::Intger(i) => Ok(Expression::Int(*i)),
            Token::IDENT(s) => Ok(Expression::Iden(s.to_owned())),
            Token::Minus => {
                self.next();
                Ok(Expression::Prefix {
                    token: PrefixToken::Minus,
                    right: Box::new(self.parse_expression(Precidence::Prefix)?),
                })
            }
            Token::Not => {
                self.next();
                Ok(Expression::Prefix {
                    token: PrefixToken::Not,
                    right: Box::new(self.parse_expression(Precidence::Prefix)?),
                })
            }

            _ => Err(anyhow!(
                "{:?} isn't implemented prefix operator. Nex token: {:?}",
                self.current_token,
                self.next_token
            )),
        }
    }
    fn parse_expression(&mut self, p: Precidence) -> anyhow::Result<Expression> {
        let mut left_exp = self.parse_prefix()?;
        println!("Parsed Prefix: {:?}", left_exp);
        println!("");
        println!("Peek prec: {:?}", self.next_token_precidence());
        while self.next_token != Token::SemiColon || p < self.next_token_precidence() {
            println!(
                "Iterating c: {:?}, n: {:?}",
                self.current_token, self.next_token
            );
            left_exp = self.parse_infix(Box::new(left_exp))?;
            self.next();
        }
        Ok(left_exp)
    }
    fn parse_statement(&mut self) -> anyhow::Result<Statement> {
        match self.current_token {
            Token::Let => {
                self.next();
                let name = match std::mem::take(&mut self.current_token) {
                    Token::IDENT(i) => Identifier(i),
                    _ => {
                        return Err(anyhow!(
                            "Let must be followed by identifier. Got {:?}",
                            self.current_token
                        ))
                    }
                };
                self.next();
                if self.current_token != Token::Assign {
                    return Err(anyhow!("Identifier must be followed by assign."));
                }
                self.next();
                let value = self
                    .parse_expression(Precidence::Lowest)
                    .context("Couldn't parse expression. After let statement")?;
                let out = Ok(Statement::Let(LetStatement::new(name, value)));
                while let Some(t) = self.next() {
                    if *t == Token::SemiColon {
                        break;
                    }
                }
                out
            }
            Token::Return => {
                self.next();

                let exp = self.parse_expression(Precidence::Lowest)?;
                println!("Expression: {:?}", exp);
                let out: anyhow::Result<Statement> =
                    Ok(Statement::Return(ReturnStatement::new(exp)));
                while let Some(t) = self.next() {
                    if *t == Token::SemiColon {
                        break;
                    }
                }
                out
            }
            _ => {
                let exp = self.parse_expression(Precidence::Lowest)?;
                let stmt_token = std::mem::take(&mut self.current_token);
                let stmt = ExpressionStatement::new(stmt_token, exp);
                return Ok(Statement::Expression(stmt));
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
                Ok(s) => statements.push(s),
                Err(e) => {
                    eprintln!("{}", e);
                    errors.push(e);
                }
            };
            self.next();
        }
        Ok(Program { statements, errors })
    }
    fn current_token_precidence(&self) -> Precidence {
        (&self.current_token).into()
    }
    fn next_token_precidence(&self) -> Precidence {
        (&self.next_token).into()
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
        let tests = vec!["let x", "let x 5", "let =5"];
        for t in tests {
            let lexer = Lexer::new(t);
            let p = Parser::new(&lexer).parse_program().unwrap();
            assert!(p.errors.len() > 0, "{:?}", p.errors)
        }
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
        let input = "fubar;\n1;";
        let lexer = Lexer::new(input);
        let p = Parser::new(&lexer).parse_program().unwrap();
        assert_eq!(p.statements.len(), 2, "Expected one expression statement.");
        assert_eq!(
            p.statements[0].get_expression().unwrap().expression,
            Expression::Iden(String::from("fubar"))
        );
        assert_eq!(
            p.statements[1].get_expression().unwrap().expression,
            Expression::Int(1)
        );
    }
    #[test]
    fn test_expression_statement_int() {
        let input = "5;";
        let lexer = Lexer::new(input);
        let p = Parser::new(&lexer).parse_program().unwrap();
        assert_eq!(p.statements.len(), 1, "Expected one expression statement.");
        match &p.statements[0] {
            Statement::Expression(a) => assert!(a.expression == Expression::Int(5)),
            _ => panic!("Wrong statement type."),
        }
    }
    #[test]
    fn test_parse_prefix() {
        let input = "-5;!10;";
        let outs = vec![(Token::Minus, 5), (Token::Not, 10)];
        let lexer = Lexer::new(input);
        let p = Parser::new(&lexer).parse_program().unwrap();
        for (stmt, o) in p.statements.iter().zip(outs) {
            assert_eq!(
                stmt.get_expression().unwrap().expression,
                Expression::Prefix {
                    token: o.0.try_into().unwrap(),
                    right: Box::new(Expression::Int(o.1))
                }
            );
            assert_eq!(stmt.token_literal().get_int().unwrap(), o.1);
        }
    }
    fn test_infix_result(in_: &str, out: (usize, &str, usize)) {
        let lexer = Lexer::new(in_);
        let p = Parser::new(&lexer).parse_program().unwrap();
        let e = p.statements[0].get_expression().unwrap();
        match e.expression.clone() {
            Expression::Infix {
                operator_token,
                left,
                right,
            } => {
                assert_eq!(operator_token.to_string(), out.1);
                assert_eq!(left.get_int().unwrap(), out.0);
                assert_eq!(right.get_int().unwrap(), out.2);
            }
            _ => panic!("{:?} Failed", e),
        }
    }
    #[test]
    fn test_parse_infix() {
        // Currently this test is failing because the prefix is parsed, then
        // next isn't called to advance the parser..
        let tests = vec![("5+5;", (5, "+", 5))];
        for (in_, out_) in tests {
            test_infix_result(in_, out_)
        }
    }
}
