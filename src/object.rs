use std::{collections::HashMap, fmt::Display};

use crate::ast::{BlockStatement, Identifier, Node, OperatorToken, PrefixToken};

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Int(isize),
    Str(String),
    Bool(bool),
    Fn(FnObject),
    Null,
}

impl Object {
    pub fn get_int(&self) -> Option<isize> {
        match *self {
            Self::Int(i) => Some(i),
            _ => None,
        }
    }
    pub fn get_bool(&self) -> Option<bool> {
        match *self {
            Self::Bool(b) => Some(b),
            _ => None,
        }
    }
    pub fn get_fn(&self) -> Option<FnObject> {
        match self {
            Self::Fn(f) => Some(f.clone()),
            _ => None,
        }
    }
    pub fn _type(&self) -> String {
        String::from(match self {
            Self::Int(_) => "INT",
            Self::Null => "NULL",
            Self::Bool(_) => "BOOL",
            Self::Str(_) => "STR",
            Self::Fn(_) => "Fn",
        })
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct FnObject {
    pub variables: Vec<Identifier>,
    pub body: Box<BlockStatement>,
    pub env: Environment,
}
pub fn eval(node: impl Node, e: &mut Environment) -> anyhow::Result<Object> {
    node.to_object(e)
}
impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Null => write!(f, "None"),
            Object::Str(s) => write!(f, "{}", s),
            Object::Fn(fn_) => write!(f, "{:?}", fn_),
        }
    }
}
#[derive(thiserror::Error, Debug)]
pub enum OjbectError {
    #[error("Type mismatch: {} {} {}", left._type(), operator.to_string(), right._type())]
    TypeMismatch {
        left: Object,
        right: Object,
        operator: OperatorToken,
    },
    #[error("Unknown Operator {} {}", operator.to_string(), left._type())]
    UnknownOperator { left: Object, operator: PrefixToken },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub store: HashMap<Identifier, Object>,
}
impl Environment {
    pub fn get(&self, key: &Identifier) -> Option<&Object> {
        self.store.get(key)
    }
    pub fn insert(&mut self, key: Identifier, value: Object) -> Option<Object> {
        self.store.insert(key, value)
    }
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::Program;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse_string(in_: &str) -> Program {
        let lexer = Lexer::new(in_);
        Parser::new(&lexer).parse_program().unwrap()
    }

    #[test]
    fn test_eval_op_fail() {
        let tests = vec!["!5", "-true"];
        for in_ in tests {
            assert!(eval(parse_string(in_), &mut Environment::new()).is_err())
        }
    }
    #[test]
    fn test_infix_op() {
        let tests = vec![
            ("5", 5),
            ("5*5", 25),
            ("10/2", 5),
            ("10 +2", 12),
            ("1-2", -1),
            ("1 + 2 * 3", 7),
            ("(1 + 2) * 3", 9),
            ("(1 + 2) / 3 * 3", 3),
            ("if (5 < 10) { 10 }", 10),
            ("if (false) {10} else {5}", 5),
            ("if (true) {10}", 10),
            ("-5", -5),
            ("-10", -10),
            ("--5", 5),
        ];
        for (in_, o) in tests {
            let p = eval(parse_string(in_), &mut Environment::new())
                .unwrap()
                .get_int()
                .unwrap();
            assert_eq!(p, o, "Failed case {}", in_);
        }
    }
    #[test]
    fn test_infix_op_bool() {
        let tests = vec![
            ("false == 1", false),
            ("5 > 10", false),
            ("! (5 < 10)", false),
            ("1==1", true),
            ("true == true", true),
            ("true == false", false),
            ("true != 1", true),
            ("false", false),
            ("true", true),
            ("!!true", true),
        ];
        for (in_, o) in tests {
            let p = eval(parse_string(in_), &mut Environment::new())
                .unwrap()
                .get_bool()
                .unwrap();
            assert_eq!(p, o, "Failed case {}", in_);
        }
    }
    #[test]
    fn test_if_false_null() {
        let in_ = "if (false) { true }";
        let p = eval(parse_string(in_), &mut Environment::new()).unwrap();
        assert_eq!(p, Object::Null, "Failed case {}", in_);
    }
    #[test]
    fn test_return_value() {
        let tests = vec![
            ("return 10;", 10),
            ("9; return 10;", 10),
            ("return 10; 15;", 10),
            ("if (true) { if (false) {return 10;}}; return 1;", 1),
        ];
        for (in_, o) in tests {
            let p = eval(parse_string(in_), &mut Environment::new())
                .unwrap()
                .get_int()
                .unwrap();
            assert_eq!(p, o, "Failed case {}", in_);
        }
    }
    fn test_operator_error(error_code: (&str, &str, &str), mut error: anyhow::Error) {
        let e = error.downcast::<OjbectError>().unwrap();
        match &e {
            OjbectError::TypeMismatch {
                left,
                right,
                operator,
            } => {
                assert_eq!(error_code.0, "OP");
                assert_eq!(left._type(), error_code.1);
                assert_eq!(right._type(), error_code.2);
            }
            OjbectError::UnknownOperator { left, operator } => {
                assert_eq!(error_code.0, "PRE");
                assert_eq!(error_code.1, left._type());
                assert_eq!(error_code.2, "");
            }

            _ => panic!("Wrong type error {}", e),
        }
    }
    #[test]
    fn test_error_value() {
        let tests = vec![
            ("5 + true", ("OP", "INT", "BOOL")),
            ("true + false", ("OP", "BOOL", "BOOL")),
            ("-true", ("PRE", "BOOL", "")),
            ("!5", ("PRE", "INT", "")),
            ("5; true + false; 10;", ("OP", "BOOL", "BOOL")),
            ("if (true) {5; true + false; 5;}", ("OP", "BOOL", "BOOL")),
        ];
        for (in_, e_code) in tests {
            let error = eval(parse_string(in_), &mut Environment::new()).unwrap_err();
            test_operator_error(e_code, error);
        }
    }
    #[test]
    fn test_let_evaluation() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5; let b = a; b", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for (in_, out) in tests {
            let out = eval(parse_string(in_), &mut Environment::new()).unwrap();
            assert_eq!(out.to_string(), format!("{}", out))
        }
    }
    #[test]
    fn test_fn_evaluation() {
        let tests = vec![
            ("let add = fn(x, y) { return x + y; }; add(1 , 2);", 3),
            (
                "let add = fn(x, y) { return x + y; }; add(1 , add(1, 2));",
                4,
            ),
            ("let two = fn() { return 2;}; two()", 2),
            ("let two = fn() { 2;}; two()", 2),
            (
                "let a = fn(x, y) {return x + y;};let b = fn() {return 2;}; a(b(), 2)",
                4,
            ),
            (
                "let two = fn(){2;};let four = fn(){two() + two();}; four()",
                4,
            ),
            ("let addr = fn(x) {fn (y) { x + y;};}; addr(2);", 4),
        ];
        for (in_, out) in tests {
            let out = eval(parse_string(in_), &mut Environment::new()).unwrap();
            assert_eq!(out.to_string(), format!("{}", out), "{}", in_)
        }
    }
}
