use std::io::{self, BufRead, Write};

pub mod ast;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;
use ast::Node;
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
        println!("{}", p.to_string());
        for e in p.errors {
            eprintln!("{}", e);
        }
    }
}
