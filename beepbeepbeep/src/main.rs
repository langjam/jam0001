use std::{env, fs::read_to_string};


mod parser;
mod interpreter;

fn main() {
    let path = env::args().skip(1).next().expect("Provide a path to a todo file");
    let source = read_to_string(path).expect("Couldn't read the file, or it's invalid UTF-8");
    interpreter::run(parser::parse(&source));
}