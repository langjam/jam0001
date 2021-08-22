use std::{env, fs};

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod interpreter;
mod model;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Some(target) = args.get(1) {
        let f = fs::read_to_string(target);
        if let Ok(source) = f {
            let toks = grammar::ProgramParser::new().parse(&source).unwrap();
            let (mut ctx, terms) = interpreter::describe(toks);
            interpreter::run(&mut ctx, terms);
        } else {
            println!("Error reading file: {:?}", f);
        }
    }
}
