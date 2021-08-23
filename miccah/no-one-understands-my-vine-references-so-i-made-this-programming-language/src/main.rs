use no_one_understands_my_vine_references_so_i_made_this_programming_language::{Lexer, Vm};
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Expected exactly one argument");
        std::process::exit(1);
    }

    match fs::read_to_string(args.get(1).unwrap()) {
        Ok(program) => {
            let lexer = Lexer::new(&program);
            let mut vm = Vm::new();
            vm.run(lexer.collect());
        }
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    }
}
