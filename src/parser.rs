use anyhow::{anyhow, bail, Context};

use crate::ast::{BlockStatement, Expression, Identifier, Node, PrefixToken, Program, Statement};
use crate::lexer::{Lexer, LexerIterator};
use crate::token::{Precidence, Token};

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
    fn parse_call_statement(&mut self) -> anyhow::Result<Expression> {
        assert_eq!(self.next_token, Token::Lparen);
        let cur_token = std::mem::take(&mut self.current_token);
        let name = match cur_token {
            Token::IDENT(i) => Identifier(i),
            _ => panic!(
                "Parse call statements must be called on identifier got {:?}",
                cur_token
            ),
        };
        let mut variables = vec![];
        self.next(); // advance to (.
        while self.next_token != Token::Rparen && self.next_token != Token::EOF {
            self.next(); // advance to iden.
            variables.push(Box::new(self.parse_expression(Precidence::Lowest)?));
            if self.next_token == Token::Comma {
                self.next(); // advance past comma.
            }
        }
        self.next(); // advance to ).
        assert_eq!(self.current_token, Token::Rparen);
        Ok(Expression::Call { name, variables })
    }
    fn parse_block_statement(&mut self) -> anyhow::Result<BlockStatement> {
        let mut block = BlockStatement::new();
        assert_eq!(self.current_token, Token::Lbrakcet);
        while self.next_token != Token::Rbracket && self.next_token != Token::EOF {
            self.next();
            block.add_statement(self.parse_statement()?);
        }
        self.next();
        assert_eq!(self.current_token, Token::Rbracket);
        Ok(block)
    }
    fn parse_if_expression(&mut self) -> anyhow::Result<Expression> {
        if self.next_token != Token::Lparen {
            return Err(anyhow!("If must be followed by (."));
        }
        self.next();
        let condition = self.parse_expression(Precidence::Lowest)?;
        if self.next_token != Token::Lbrakcet && self.current_token == Token::Rparen {
            bail!(
                "If (condition) {{}} must be followed by a {{ got {:?}, then {:?}.",
                self.current_token,
                self.next_token,
            );
        }
        self.next();
        let consequence = self
            .parse_block_statement()
            .context("parsing block statement failed.")?;
        println!("Post Consequence current token: {:?}", self.current_token);
        self.next();
        let alternative = match self.current_token {
            Token::SemiColon | Token::EOF => None,
            Token::Else => {
                if self.next_token != Token::Lbrakcet {
                    return Err(anyhow!(
                        "Else must be followed by a {{ got {:?}.",
                        self.next_token
                    ));
                };
                self.next();
                Some(Box::new(
                    self.parse_block_statement()
                        .context("parsing block statement failed.")?,
                ))
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
        let block = self
            .parse_block_statement()
            .context("Parsing block statement failed.")?;
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
        self.next();
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
            Token::IDENT(s) => match self.next_token {
                Token::Lparen => self.parse_call_statement(),
                _ => Ok(Expression::iden_from_str(s)),
            },
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
    /// Parse the expression type.
    /// Parses an expression up to the next token being a semi colon
    /// or the next token having higher Precidence.
    fn parse_expression(&mut self, p: Precidence) -> anyhow::Result<Expression> {
        let mut left_exp = self
            .parse_prefix()
            .context("Failed parsing prefix, during expression parsing.")?;
        while self.next_token != Token::SemiColon && p < self.next_token_precidence() {
            self.next();
            left_exp = self
                .parse_infix(Box::new(left_exp))
                .context("Failed parsing infix, during expression parsing.")?;
        }
        Ok(left_exp)
    }
    /// Parse the statement, end the evaluation at a semi colon or a eof apart from expression.
    /// Expression statements can stay on their last item, but attemt to advance.
    fn parse_statement(&mut self) -> anyhow::Result<Statement> {
        let out = match self.current_token {
            Token::Let => {
                self.next(); // move onto ident.
                let name = match std::mem::take(&mut self.current_token) {
                    Token::IDENT(i) => Identifier(i),
                    _ => {
                        return Err(anyhow!(
                            "Let must be followed by identifier. Got {:?}",
                            self.current_token
                        ))
                    }
                };
                self.next(); // move onto equals.
                if self.current_token != Token::Assign {
                    return Err(anyhow!("Identifier must be followed by assign."));
                }
                self.next(); // move onto expression start.
                let expression = self
                    .parse_expression(Precidence::Lowest)
                    .context("During parsing let statement")?;
                let out = Ok(Statement::Let(name, expression));
                self.next(); // move onto semicolon;
                out
            }
            Token::Return => {
                self.next(); // move past return token.
                let exp = self
                    .parse_expression(Precidence::Lowest)
                    .context("During parsing return statament.")?;
                let out: anyhow::Result<Statement> = Ok(Statement::Return(exp));
                self.next(); // move onto semicolon.
                out
            }
            _ => {
                let exp = self
                    .parse_expression(Precidence::Lowest)
                    .context("During parsing expression statement.")?;
                if self.next_token == Token::SemiColon {
                    self.next();
                }
                return Ok(Statement::Expression(exp));
            }
        };

        // if self.current_token == Token::SemiColon || self.current_token == Token::EOF {
        //     Err(anyhow!(
        //         "Invalid final token, expected a ; or the end of a file. Got {:?}",
        //         self.current_token
        //     ));
        // }
        out
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
    fn test_let_statemnt() {
        let in_ = "let x = 5;";
        let p = parse_string(in_);
        assert_eq!(p.statements[0].get_let().unwrap().to_string(), "5");
    }

    #[test]
    fn test_let_statements() {
        let in_ = "let x = 5;\nlet y = 10;\nlet boobies = 500535;";
        let out = vec!["5", "10", "500535"];
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 3, "{:?}", p.statements);
        let ident_tests = vec!["x", "y", "boobies"];
        for ((expected_ident, stmt), o) in ident_tests.iter().zip(p.statements).zip(out) {
            match stmt {
                Statement::Let(i, e) => {
                    assert_eq!(i.to_string(), *expected_ident);
                    assert_eq!(e.to_string(), o);
                }
                _ => panic!(),
            }
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
        let out = vec!["5", "15", "x"];
        let p = parse_string(in_);

        assert_eq!(p.statements.len(), 3);
        for (stmt, o) in p.statements.iter().zip(out) {
            match stmt {
                Statement::Return(r) => assert_eq!(r.to_string(), o),
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
            *p.statements[0].get_expression().unwrap(),
            Expression::iden_from_str("fubar")
        );
        assert_eq!(
            *p.statements[1].get_expression().unwrap(),
            Expression::Int(1)
        );
    }
    #[test]
    fn test_expression_statement_int() {
        let in_ = "5;";
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 1, "Expected one expression statement.");
        match &p.statements[0] {
            Statement::Expression(a) => assert!(*a == Expression::Int(5)),
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
                *stmt.get_expression().unwrap(),
                Expression::Prefix {
                    token: o.0.try_into().unwrap(),
                    right: Box::new(Expression::Int(o.1))
                }
            );
        }
    }
    fn test_infix_result(in_: &str, out: (Token, &str, Token)) {
        let p = parse_string(in_);
        let e = p.statements[0].get_expression().unwrap();
        match e.clone() {
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
            ("add(a + b, c) * d", "(add((a + b), c) * d)"),
            ("a + b + c", "((a + b) + c)"),
            ("a - b * c", "(a - (b * c))"),
            ("a * b + c / e * f", "((a * b) + ((c / e) * f))"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("!-a", "(!(-a))"),
            ("(a + b) * c", "((a + b) * c)"),
            ("5 < 4 == 3 > 4", "((5 < 4) == (3 > 4))"),
            ("!true", "(!true)"),
            ("!true == false", "((!true) == false)"),
            ("(a + (b + c))", "(a + (b + c))"),
            ("b * (a + b)", "(b * (a + b))"),
        ];
        for (i, (in_, out_)) in tests.into_iter().enumerate() {
            let p = parse_string(in_);
            assert_eq!(
                p.to_string(),
                out_,
                "Failded on test case {:?}: {:?}, {:?}",
                i,
                in_,
                p.statements[0].get_expression()
            );
        }
    }
    #[test]
    fn test_if_expression() {
        let in_ = "if (x < y) { x };";
        let p = parse_string(in_);
        assert_eq!(p.statements.len(), 1);
        let if_ = p.statements[0].get_expression().unwrap();
        match if_.clone() {
            Expression::If {
                condition,
                consequnce,
                alternative,
            } => {
                assert_eq!(condition.to_string(), "(x < y)");
                assert_eq!(consequnce.statements.len(), 1);
                assert_eq!(consequnce.to_string(), String::from("x"));
                assert!(alternative.is_none());
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
        match if_.clone() {
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
        let in_ = "fn(x,y) { let a = x + y; let z = x; return z; };";
        let p = parse_string(in_);
        // assert_eq!(p.statements.len(), 1);
        let e = p.statements[0].get_expression().unwrap();
        match &e {
            Expression::Fn { parameters, body } => {
                let expected_parameters = vec!["x", "y"];
                for (parameter, expected_parameter) in parameters.iter().zip(expected_parameters) {
                    assert_eq!(parameter.to_string(), expected_parameter);
                }
                assert_eq!(body.statements.len(), 3, "{:?}", body.statements);
            }
            _ => panic!("Expected fn."),
        }
    }
    fn test_parameters(e: &Expression, out: Vec<&str>) {
        match &e {
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
    #[test]
    fn test_call_parse_expressions() {
        let tests = vec![
            "test(x, y)",
            "test()",
            "test(1 + 2, a)",
            "test(a, 1 + 2 * 3)",
        ];
        for t in tests {
            let p = parse_string(t);
            let e = p.statements[0].get_expression().unwrap();
            match e.clone() {
                Expression::Call { name, variables } => assert_eq!(name.to_string(), "test"),
                _ => panic!(),
            };
        }
    }
    #[test]
    fn test_call_parse() {
        let in_ = "test(x, y)";
        let var = vec!["x", "y"];
        let p = parse_string(in_);
        match p.statements[0].get_expression().unwrap().clone() {
            Expression::Call { name, variables } => {
                assert_eq!(name.to_string(), "test");
                for (var, out) in variables.iter().zip(var) {
                    assert_eq!(var.to_string(), out);
                }
            }
            _ => panic!(),
        }
    }
}
