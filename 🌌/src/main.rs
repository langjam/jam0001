#![feature(try_trait_v2)]
mod ast;
mod builtins;
mod interpreter;
mod lexer;
mod parser;

use std::{
    env, fs,
    io::{self, Read, Result},
};

use interpreter::{Exec, Interpreter};
use lexer::Lexer;
use parser::Parser;

fn process(code: &str) {
    let lexer = Lexer::new(code);
    match lexer.into_tokens() {
        Ok(tokens) => {
            let parser = Parser::new(tokens);
            match parser.parse() {
                Ok(expression) => {
                    let mut interpreter = Interpreter::with_ast(&expression);
                    match interpreter.run() {
                        Exec::Err(e) => eprintln!("Interpreter error: {}", e),
                        Exec::Return(_) => eprintln!("return outside of a function"),
                        Exec::Break => eprintln!("break outside of a loop"),
                        Exec::Continue => eprintln!("continue outside of a loop"),
                        Exec::Ok => (),
                    }
                }
                Err(e) => eprintln!("Parser error: {}", e),
            }
        }
        Err(e) => {
            eprintln!("Lexer error: {}", e);
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    if args.len() == 1 {
        let mut code = String::new();
        io::stdin().read_to_string(&mut code)?;
        process(&code);
    } else if args.len() == 2 {
        let code = fs::read_to_string(&args[1])?;
        process(&code);
    } else {
        eprintln!("Usage: {} [<filename>]", args[0]);
    }

    Ok(())
}
