use core::str;

use anyhow::anyhow ;

use crate::object::{Environment, FnObject, Object, OjbectError};
use crate::token::Token;

pub trait Node {
    fn to_string(&self) -> String;
    fn to_object(&self, e: &mut Environment) -> anyhow::Result<Object>;
}
fn objectify_ordered_statements(v: &Vec<Statement>, e: &mut Environment) -> anyhow::Result<Object> {
    let mut result = Object::Null;
    for stmt in v.iter() {
        if let Statement::Return(x) = stmt {
            return x.to_object(e);
        }
        result = stmt.to_object(e)?
    }
    Ok(result)
}
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<anyhow::Error>,
}
impl Node for Program {
    fn to_string(&self) -> String {
        let mut out = String::new();
        for stmt in self.statements.iter() {
            out.push_str(&stmt.to_string());
        }
        out
    }
    fn to_object(&self, e: &mut Environment ) -> anyhow::Result<Object> {
        objectify_ordered_statements(&self.statements, e)
    }
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
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}
impl BlockStatement {
    pub fn new() -> BlockStatement {
        BlockStatement { statements: vec![] }
    }
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement)
    }
}
impl Node for BlockStatement {
    fn to_string(&self) -> String {
        let mut out = String::new();
        for stmt in self.statements.iter() {
            out.push_str(&*stmt.to_string())
        }
        out
    }
    fn to_object(&self, e: &mut Environment) -> anyhow::Result<Object> {
        objectify_ordered_statements(&self.statements, e)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    None,
    Prefix {
        token: PrefixToken,
        right: Box<Expression>,
    },
    True,
    False,
    Iden(Identifier),
    Int(usize),
    Infix {
        operator_token: OperatorToken,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequnce: Box<BlockStatement>,
        alternative: Option<Box<BlockStatement>>,
    },
    Fn {
        parameters: Vec<Identifier>,
        body: Box<BlockStatement>,
    },
    Call {
        name: Identifier,
        variables: Vec<Box<Expression>>,
    },
}
impl Expression {
    pub fn iden_from_str(s: impl Into<String>) -> Expression {
        Expression::Iden(Identifier(s.into()))
    }
    pub fn get_int(&self) -> Option<usize> {
        match self {
            Self::Int(i) => Some(*i),
            _ => None,
        }
    }
}
impl Node for Expression {
    fn to_string(&self) -> String {
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
            Expression::Iden(i) => format!("{}", i.to_string()),
            Expression::True => String::from("true"),
            Expression::False => String::from("false"),
            Expression::If {
                condition,
                consequnce,
                alternative,
            } => match alternative {
                Some(a) => {
                    format!(
                        "if {} {} else {};",
                        condition.to_string(),
                        consequnce.to_string(),
                        a.to_string(),
                    )
                }
                None => format!("if {} {};", condition.to_string(), consequnce.to_string()),
            },
            Expression::Fn { parameters, body } => {
                let s: String = parameters
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("fn({}){{ {} }}", s, body.to_string())
            }
            Expression::Call { name, variables } => {
                let s: String = variables
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}({})", name.to_string(), s)
            }
            Expression::None => String::new(),
        }
    }
    fn to_object(&self, e: &mut Environment) -> anyhow::Result<Object> {
        match self {
            Expression::Int(i) => Ok(Object::Int(*i as isize)),
            Expression::True => Ok(Object::Bool(true)),
            Expression::False => Ok(Object::Bool(false)),
            Expression::Prefix { token, right } => match (token, right.to_object(e)?) {
                (PrefixToken::Not, Object::Bool(b)) => Ok(Object::Bool(!b)),
                (PrefixToken::Minus, Object::Int(i)) => Ok(Object::Int(-i)),
                _ => Err(anyhow::Error::new(OjbectError::UnknownOperator { left: right.to_object(e)?, operator: token.clone() })),
                
            },
            // Would make OperatorToken implement infix, then adding new operators is easy.
            Expression::Infix {
                operator_token,
                left,
                right,
            } => match (left.to_object(e)?, operator_token, right.to_object(e)?) {
                //IDENTITY
                (Object::Null, OperatorToken::Equal, Object::Null) => Ok(Object::Bool(true)),
                // INT
                (Object::Int(i), OperatorToken::Minus, Object::Int(j)) => Ok(Object::Int(i - j)),
                (Object::Int(i), OperatorToken::Plus, Object::Int(j)) => Ok(Object::Int(i + j)),
                (Object::Int(i), OperatorToken::Mul, Object::Int(j)) => Ok(Object::Int(i * j)),
                (Object::Int(i), OperatorToken::Div, Object::Int(j)) => Ok(Object::Int(i / j)),
                (Object::Int(i), OperatorToken::Equal, Object::Int(j)) => Ok(Object::Bool(i == j)),
                (Object::Int(i), OperatorToken::NotEqual, Object::Int(j)) => {
                    Ok(Object::Bool(i != j))
                }
                (Object::Int(i), OperatorToken::Larrow, Object::Int(j)) => Ok(Object::Bool(i < j)),
                (Object::Int(i), OperatorToken::Rarrow, Object::Int(j)) => Ok(Object::Bool(i > j)),

                // BOOL
                (Object::Bool(i), OperatorToken::Equal, Object::Bool(j)) => {
                    Ok(Object::Bool(i == j))
                }
                (Object::Bool(i), OperatorToken::NotEqual, Object::Bool(j)) => {
                    Ok(Object::Bool(i != j))
                }
                (Object::Str(s), OperatorToken::Equal, Object::Str(t)) => Ok(Object::Bool(s == t)),
                (Object::Str(s), OperatorToken::NotEqual, Object::Str(t)) => Ok(Object::Bool(s != t)),
                (Object::Str(mut  s), OperatorToken::Plus, Object::Str(t)) => {
                    Ok(Object::Str({s.push_str(&*t); s}))
                },
                // FALLTHROUGH ==
                (Object::Int(_), OperatorToken::Equal, _)
                | (_, OperatorToken::Equal, Object::Int(_)) => Ok(Object::Bool(false)),
                (Object::Bool(_), OperatorToken::Equal, _)
                | (_, OperatorToken::Equal, Object::Bool(_)) => Ok(Object::Bool(false)),
                (Object::Null, OperatorToken::Equal, _)
                | (_, OperatorToken::Equal, Object::Null) => Ok(Object::Bool(false)),
                (Object::Str(_), OperatorToken::Equal, _) // last one out patter looks nice but
                // isn't necessary
                | (_, OperatorToken::Equal, Object::Str(_)) => Ok(Object::Bool(false)),

                // FALLTHROUGH !=
                (Object::Int(_), OperatorToken::NotEqual, _)
                | (_, OperatorToken::NotEqual, Object::Int(_)) => Ok(Object::Bool(true)),
                (Object::Bool(_), OperatorToken::NotEqual, _)
                | (_, OperatorToken::NotEqual, Object::Bool(_)) => Ok(Object::Bool(true)),
                (Object::Null, OperatorToken::NotEqual, _)
                | (_, OperatorToken::NotEqual, Object::Null) => Ok(Object::Bool(true)),
                (Object::Str(s), OperatorToken::NotEqual, _)
                | (_, OperatorToken::NotEqual, Object::Str(s)) => Ok(Object::Bool(true)),
                _=> Err(
                    anyhow::Error::new(OjbectError::TypeMismatch { 
                        left: left.to_object(e)?, 
                        right: right.to_object(e)?, 
                        operator: operator_token.clone() 
                    })),
            }
            Expression::If {
                condition,
                consequnce,
                alternative,
            } => {
                let cond = condition.to_object(e)?.get_bool().ok_or(anyhow!(
                    "{} Condition must evaluate to a boolean",
                    condition.to_string()
                ))?;
                if cond {
                    consequnce.to_object(e)
                } else {
                    match alternative {
                        None => Ok(Object::Null),
                        Some(bs) => bs.to_object(e),
                    }
                }
            }
            Expression::Iden(i) => {
                let Some(o) = e.get(i) else {
                    return Ok(Object::Null);
                };
                Ok(o.clone())

            }
            Expression::Fn { parameters, body } => {
                Ok(Object::Fn (FnObject{ variables: parameters.clone(), body: body.clone() , env: Environment::new() }))
                    
            }
            Expression::Call { name, variables } => {
                let Some(fn_ )= e.get(name) else {
                    return Err(anyhow!("No variable {}", name.0));
                };
                let Some(mut fn_) = fn_.get_fn() else {
                    return Err(anyhow!("No function found with Identifier {}", name.0))
                };
                for (idn, var,) in fn_.variables.into_iter().zip(variables) {
                    fn_.env.insert(idn, var.to_object(e)?);
                }
                for (idn, var) in e.store.iter() {
                    fn_.env.insert(idn.clone(), var.clone());
                }
                fn_.body.to_object(&mut fn_.env)
            }


            _ => todo!(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}
impl Node for Statement {
    fn to_string(&self) -> String {
        match self {
            Self::Let(_, e) => e.to_string(),
            Self::Return(s) => s.to_string(),
            Self::Expression(s) => s.to_string(),
        }
    }
    fn to_object(&self, e: &mut Environment) -> anyhow::Result<Object> {
        match self {
            Self::Expression(x) => x.to_object(e),
            Self::Return(x) => x.to_object(e),
            Self::Let(i, x) => {
                let o = x.to_object(e)?;
                e.insert(i.clone(), o);
                Ok(Object::Null)
            },
        }
    }
}
impl Statement {
    pub fn get_expression(&self) -> Option<&Expression> {
        match self {
            Self::Expression(a) => Some(a),
            _ => None,
        }
    }
    pub fn get_let(&self) -> Option<&Expression> {
        match self {
            Self::Let(_, a) => Some(a),
            _ => None,
        }
    }
}
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Identifier(pub String);
impl From<String> for Identifier{
    fn from(value: String) -> Self {
        Identifier(value)
    }
}
impl From<&str> for Identifier {
    fn from(value: &str)  -> Identifier {
        Identifier(value.to_string())
    }
}

impl Node for Identifier {
    fn to_string(&self) -> String {
        self.0.clone()
    }
    fn to_object(&self, e: &mut Environment) -> anyhow::Result<Object> {
        todo!()
    }
}
