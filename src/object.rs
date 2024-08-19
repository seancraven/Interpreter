use crate::ast::Node;

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Int(usize),
    Str,
    Bool,
    Null,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    _type: Type,
}

fn eval(node: impl Node) -> Object {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::{Parser, Program};

    fn test_integer_literal(o: Object, e: usize) {
        match o._type {
            Type::Int(i) => assert_eq!(i, e),
            _ => panic!("Expected integer."),
        }
    }
    fn parse_string(in_: &str) -> Program {
        let lexer = Lexer::new(in_);
        Parser::new(&lexer).parse_program().unwrap()
    }
    #[test]
    fn test_eval_integer() {
        let tests = vec![("5", 5), ("10", 10)];
        for (in_, o) in tests {
            let p = eval(parse_string(in_));
        }
    }
}
