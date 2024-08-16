use std::io::{self, BufRead, Write};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

fn main() -> io::Result<()> {
    println!("Hello welcome to monkey repl!");
    let mut handle = io::stdin().lock();
    loop {
        let mut buf = String::new();
        print!(">>");
        io::stdout().flush()?;
        handle.read_line(&mut buf)?;
        let mut l = lexer::Lexer::new(buf);
        let p = parser::Parser::new(&l).parse_program().unwrap();
        for token in l.into_iter() {
            println!("{:?}", token);
        }
    }
}
