# Compiler and Interpreter Implementation In Rust

I had the desire to learn how programmign languages are implemented.
This repo follows the implementation of Torsten Ball's excellent books [Writing An Interpreter In Go](https://interpreterbook.com/) and [Writing a Compiler in Go](https://compilerbook.com).

It's a simple implementation of the monkey interpreter and compiler.

## Usage
```bash
# run the interpreter in repl mode:
cargo run 
# Run the interpeter on a file:
cargo run -- path/to/file
```
There is an example script of the language, in `monkey.mnk`, for syntax and testing reference.

## Features
1. Functions as values.
2. Integers, Booleans.
3. Conditionals.
2. Addition and multiplication, and their inverse operators.

## Not Features
1. Loops.
2. Complex data structures.
