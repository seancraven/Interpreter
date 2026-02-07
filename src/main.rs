use std::{
    fs::File,
    io::{self, BufRead, Read, Write},
    path::PathBuf,
};

pub mod ast;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;
use anyhow::Result;
use ast::Node;
use clap::Parser;
use log::debug;
use object::{Environment, Object};
fn main() -> Result<()> {
    let args = Args::parse();

    match args.file {
        None => repl().unwrap(),
        Some(path) => {
            let path_buf = PathBuf::try_from(&path).unwrap();
            if !(path_buf.exists() && path_buf.is_file()) {
                panic!("{:?} isn't a valid file.", path);
            }
            let mut buf = String::new();
            debug!("Running {:?}", &path_buf);
            File::open(path_buf)?.read_to_string(&mut buf)?;
            debug!("{}", buf);
            let mut env = Environment::new();
            run_program(buf, &mut env)?;
        }
    };
    Ok(())
}

fn run_program(program: String, env: &mut Environment) -> Result<()> {
    let l = lexer::Lexer::new(program);
    let p = parser::Parser::new(&l).parse_program().unwrap();
    let eval = p.to_object(env).unwrap();
    if eval != Object::Null {
        println!("{}", eval);
    }

    for e in p.errors {
        eprintln!("{}", e);
    }
    Ok(())
}

fn repl() -> Result<()> {
    println!("Hello welcome to monkey repl!");
    let mut handle = io::stdin().lock();
    let mut env = Environment::new();
    loop {
        let mut buf = String::new();
        print!(">>");
        io::stdout().flush()?;
        handle.read_line(&mut buf)?;
        run_program(buf, &mut env)?;
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
struct Args {
    file: Option<String>,
}
