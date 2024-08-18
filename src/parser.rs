use anyhow::{anyhow, Context};

use crate::ast::{
    BlockStatement, Expression, ExpressionStatement, Identifier, LetStatement, Node, PrefixToken,
    ReturnStatement, Statement,
};
use crate::lexer::{Lexer, LexerIterator};
use crate::token::{Precidence, Token};

pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<anyhow::Error>,
}
impl Program {
    pub fn to_string(&self) -> String {
        let mut out = String::new();
        for stmt in self.statements.iter() {
            out.push_str(&stmt.to_string());
        }
        out
    }
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
    fn parse_block_statement(&mut self) -> anyhow::Result<BlockStatement> {
        let mut block = BlockStatement::new();
        assert_eq!(self.current_token, Token::Lbrakcet);
        self.next();
        if self.current_token != Token::Rbracket && self.current_token != Token::EOF {
            block.add_statement(self.parse_statement()?);
            self.next();
        }
        Ok(block)
    }
    fn parse_if_expression(&mut self) -> anyhow::Result<Expression> {
        if self.next_token != Token::Lparen {
            return Err(anyhow!("If must be followed by (."));
        }
        self.next();
        let condition = self.parse_expression(Precidence::Lowest)?;
        if self.next_token != Token::Rparen {
            return Err(anyhow!(
                "If expressions must contain full conditions ending in a ) got {:?}.",
                self.next_token
            ));
        }
        self.next();
        if self.next_token != Token::Lbrakcet {
            return Err(anyhow!(
                "If (condition) must be followed by a {{ got {:?}.",
                self.next_token
            ));
        }
        self.next();
        let consequence = self.parse_block_statement()?;
        self.next();
        let alternative = match self.current_token {
            Token::SemiColon => None,
            Token::Else => {
                if self.next_token != Token::Lbrakcet {
                    return Err(anyhow!(
                        "Else must be followed by a {{ got {:?}.",
                        self.next_token
                    ));
                };
                self.next();
                Some(Box::new(self.parse_block_statement()?))
            }
            _ => Err(anyhow!(
                "If consequence, must be followed by an else expression or a SemiColon, got {:?}",
                self.next_token
            ))?,
        };

        Ok(Expression::If {
            condition: Box::new(condition),
            consequnce: Box::new(consequence),
            alternative,
        })
    }
    fn parse_fn_expression(&mut self) -> anyhow::Result<Expression> {
        if self.next_token != Token::Lparen {
            return Err(anyhow!(
                "Fn expression must start with (, got {:?}",
                self.next_token
            ));
        }
        self.next(); // advance to (.
        let mut parameters = vec![];
        while self.next_token != Token::Rparen {
            self.next();
            match self.current_token.clone() {
                Token::IDENT(i) => {
                    parameters.push(Identifier(i));
                }
                _ => Err(anyhow!(
                    "Expected an identifier, got {:?}",
                    self.current_token
                ))?,
            };
            if self.next_token == Token::Comma {
                self.next();
            }
        }
        self.next();
        assert_eq!(self.current_token, Token::Rparen);
        self.next();
        let block = self.parse_block_statement()?;
        Ok(Expression::Fn {
            parameters,
            body: Box::new(block),
        })
    }
    fn parse_grouped_expression(&mut self) -> anyhow::Result<Expression> {
        self.next();
        let exp = self.parse_expression(Precidence::Lowest)?;
        if self.next_token != Token::Rparen {
            return Err(anyhow!(
                "Expression expected to be followed by a right parenthesis. Exp: {:?} folloewd by {:?}", exp.to_string(), self.next_token
            ));
        }
        Ok(exp)
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
            Token::IDENT(s) => Ok(Expression::iden_from_str(s)),
            Token::True => Ok(Expression::True),
            Token::False => Ok(Expression::False),
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
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Fn => self.parse_fn_expression(),
            _ => Err(anyhow!(
                "{:?} isn't implemented prefix operator. Nex token: {:?}",
                self.current_token,
                self.next_token
            )),
        }
    }
    fn parse_expression(&mut self, p: Precidence) -> anyhow::Result<Expression> {
        let mut left_exp = self.parse_prefix()?;
        while self.next_token != Token::SemiColon && p < self.next_token_precidence() {
            self.next();
            left_exp = self.parse_infix(Box::new(left_exp))?;
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
                out
            }
            Token::Return => {
                self.next();

                let exp = self.parse_expression(Precidence::Lowest)?;
                let out: anyhow::Result<Statement> =
                    Ok(Statement::Return(ReturnStatement::new(exp)));
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

    use super::Expression;
    use super::*;
    use crate::ast::Node;

    fn parse_string(in_: &str) -> Program {
        let lexer = Lexer::new(in_);
        Parser::new(&lexer).parse_program().unwrap()
    }

    #[test]
    fn test_let_statement() {
        let in_ = "let x = 5;\nlet y = 10;\nlet boobies = 500535;";
        let p = parse_string(in_);
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
        for in_ in tests {
            let p = parse_string(in_);
            assert!(p.errors.len() > 0, "{:?}", p.errors)
        }
    }
    #[test]
    fn test_return_statement() {
        let in_ = "return 5;\nreturn 15;\nreturn x;";
        let p = parse_string(in_);
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
        let in_ = "fubar;\n1;";
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 2, "Expected one expression statement.");
        assert_eq!(
            p.statements[0].get_expression().unwrap().expression,
            Expression::iden_from_str("fubar")
        );
        assert_eq!(
            p.statements[1].get_expression().unwrap().expression,
            Expression::Int(1)
        );
    }
    #[test]
    fn test_expression_statement_int() {
        let in_ = "5;";
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 1, "Expected one expression statement.");
        match &p.statements[0] {
            Statement::Expression(a) => assert!(a.expression == Expression::Int(5)),
            _ => panic!("Wrong statement type."),
        }
    }
    #[test]
    fn test_parse_prefix() {
        let in_ = "-5;!10;";
        let outs = vec![(Token::Minus, 5), (Token::Not, 10)];
        let p = parse_string(in_);
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
    fn test_infix_result(in_: &str, out: (Token, &str, Token)) {
        let p = parse_string(in_);
        let e = p.statements[0].get_expression().unwrap();
        match e.expression.clone() {
            Expression::Infix {
                operator_token,
                left,
                right,
            } => {
                assert_eq!(operator_token.to_string(), out.1);
                assert_eq!(left.to_string(), out.0.to_string());
                assert_eq!(right.to_string(), out.2.to_string());
            }
            _ => panic!("{:?} Failed", e),
        }
    }
    #[test]
    fn test_parse_infix() {
        let tests = vec![
            ("5+5;", (Token::Intger(5), "+", Token::Intger(5))),
            (
                "add * dave;",
                (
                    Token::IDENT(String::from("add")),
                    "*",
                    Token::IDENT(String::from("dave")),
                ),
            ),
        ];
        for (in_, out_) in tests {
            test_infix_result(in_, out_)
        }
    }
    #[test]
    fn test_parse_precidence() {
        let tests = vec![
            ("-a * b;", "((-a) * b)"),
            ("a + b + c", "((a + b) + c)"),
            ("a - b * c", "(a - (b * c))"),
            ("a * b + c / e * f", "((a * b) + ((c / e) * f))"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("!-a", "(!(-a))"),
            ("5 < 4 == 3 > 4", "((5 < 4) == (3 > 4))"),
            ("!true", "(!true)"),
            ("!true == false", "((!true) == false)"),
            ("(a + (b + c))", "(a + (b + c))"),
            ("b * (a + b)", "(b * (a + b))"),
        ];
        for (in_, out_) in tests {
            let p = parse_string(in_);
            assert_eq!(p.to_string(), out_)
        }
    }
    #[test]
    fn test_if_expression() {
        let in_ = "if (x < y) { x };";
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 1);
        let if_ = p.statements[0].get_expression().unwrap();
        match if_.expression.clone() {
            Expression::If {
                condition,
                consequnce,
                alternative,
            } => {
                assert_eq!(condition.to_string(), "(x < y)");
                assert_eq!(consequnce.statements.len(), 1);
                assert_eq!(consequnce.to_string(), String::from("x"));
            }
            _ => panic!("shoudl be if."),
        }
    }
    #[test]
    fn test_if_else_expression() {
        let in_ = "if (x < y) { x } else { y };";
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 1);
        let if_ = p.statements[0].get_expression().unwrap();
        match if_.expression.clone() {
            Expression::If {
                condition,
                consequnce,
                alternative,
            } => {
                assert_eq!(condition.to_string(), "(x < y)");
                assert_eq!(consequnce.statements.len(), 1);
                assert_eq!(consequnce.to_string(), String::from("x"));
                assert_eq!(alternative.unwrap().to_string(), String::from("y"))
            }
            _ => panic!("shoudl be if."),
        }
    }
    #[test]
    fn test_fn_parse() {
        let in_ = "fn(x,y) { x + y; };";
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 1);
        let e = p.statements[0].get_expression().unwrap();
        match &e.expression {
            Expression::Fn { parameters, body } => {
                let expected_parameters = vec!["x", "y"];
                for (parameter, expected_parameter) in parameters.iter().zip(expected_parameters) {
                    assert_eq!(parameter.to_string(), expected_parameter);
                }
                assert_eq!(body.statements.len(), 1);
                assert_eq!(
                    body.statements[0].get_expression().unwrap().to_string(),
                    "(x + y)"
                );
            }
            _ => panic!("Expected fn."),
        }
    }
    fn test_parameters(e: &ExpressionStatement, out: Vec<&str>) {
        match &e.expression {
            Expression::Fn { parameters, body } => {
                for (p, o) in parameters.iter().zip(out) {
                    assert_eq!(p.to_string(), o)
                }
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_fn_parameter_parse() {
        let tests = vec![
            ("fn(){};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
            ("fn(x, y, z,) {};", vec!["x", "y", "z"]),
        ];
        for (in_, out) in tests {
            let p = parse_string(in_);
            let e = p.statements[0].get_expression().unwrap();
            test_parameters(e, out);
        }
    }
}
