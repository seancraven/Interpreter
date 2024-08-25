use std::io::{self, BufRead, Write};

pub mod ast;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;
use ast::Node;
use object::{Environment, Object};
fn main() -> io::Result<()> {
    println!("Hello welcome to monkey repl!");
    let mut handle = io::stdin().lock();
    loop {
        let mut buf = String::new();
        print!(">>");
        io::stdout().flush()?;
        handle.read_line(&mut buf)?;
        let l = lexer::Lexer::new(buf);
        let p = parser::Parser::new(&l).parse_program().unwrap();
        let mut env = Environment::new();
        let eval = p.to_object(&mut env).unwrap();
        if eval != Object::Null {
            println!("{}", eval);
        }

        for e in p.errors {
            eprintln!("{}", e);
        }
    }
}
