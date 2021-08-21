use std::env::args;
use std::fs::read_to_string;
use colored::*;

mod lexer;
mod token;
mod parser;
mod ast;
mod expression;
mod interpreter;
mod environment;
mod value;
mod document;
mod stdlib;

pub fn has_flag(flag: &str) -> bool {
    args().find(|i| i == flag).is_some()
}

fn error(message: &str) {
    eprintln!("{}", message.red().bold());
}

fn main() {
    let file = if let Some(f) = args().nth(1) {
        f
    } else {
        error("Please provide a filepath to execute");
        std::process::exit(1);
    };

    let contents = if let Ok(c) = read_to_string(file) {
        c
    } else {
        error("Could not read the specified file.");
        std::process::exit(1);
    };

    let tokens = match lexer::generate(&contents) {
        Ok(tl) => tl,
        Err(e) => {
            error(&format!("{:?}", e));
            std::process::exit(1);
        }
    };

    if has_flag("--dump-tokens") {
        dbg!(tokens.clone());
    }

    let program = match parser::parse(tokens.iter()) {
        Ok(p) => p,
        Err(e) => {
            error(&format!("{}", e));
            std::process::exit(1);
        }
    };

    if has_flag("--dump-ast") {
        dbg!(program.clone());
    }

    match interpreter::interpret(program) {
        Ok(_) => (),
        Err(e) => {
            error(&format!("{}", e));
            std::process::exit(1);
        }
    };
}
