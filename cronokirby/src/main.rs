mod interpreter;
mod lexer;
mod parser;

use std::fs;
use std::path::Path;
use std::path::PathBuf;

use lexer::Lexer;
use parser::Parser;
use structopt::StructOpt;

use crate::parser::DocumentChunk;

/// A command that our CLI can process
#[derive(Debug, StructOpt)]
enum Command {
    /// Print the tokens produced by the lexer.
    Lex {
        /// The file containing Wahlbergdown code you want to lex.
        #[structopt(name = "INPUT_FILE", parse(from_os_str))]
        input_file: PathBuf,
    },
    /// Print the AST produced by the parser.
    Parse {
        /// The file containing Wahlbergdown code you want to parse.
        #[structopt(name = "INPUT_FILE", parse(from_os_str))]
        input_file: PathBuf,
    },
    // Run a file.
    Run {
        /// The file containing Wahlbergdown code you want to run.
        #[structopt(name = "INPUT_FILE", parse(from_os_str))]
        input_file: PathBuf,
    },
}

fn lex_and_stop(input_file: &Path) {
    let src = fs::read_to_string(input_file).expect("failed to read input file");
    let tokens = Lexer::new(&src);
    for tok in tokens {
        println!("{:?}", tok);
    }
}

fn parse_and_stop(input_file: &Path) {
    let src = fs::read_to_string(input_file).expect("failed to read input file");
    let tokens = Lexer::new(&src);
    let chunks = Parser::new(tokens);
    for chunk in chunks {
        println!("{:?}", chunk);
    }
}

fn run(input_file: &Path) {
    let src = fs::read_to_string(input_file).expect("failed to read input file");
    let tokens = Lexer::new(&src);
    let chunks = Parser::new(tokens);
    let mut interpreter = interpreter::Interpreter::new();
    for chunk in chunks {
        match chunk {
            DocumentChunk::Raw(r) => print!("{}", r),
            DocumentChunk::Comment(c) => match interpreter.definition(c) {
                Err(e) => print!("<!--ERROR: {}-->", e),
                Ok(()) => {}
            },
            DocumentChunk::Interpolate(c) => match interpreter.expr(c) {
                Err(e) => print!("`ERROR: {}`", e),
                Ok(v) => print!("{}", v),
            },
        }
    }
}

fn main() {
    let args = Command::from_args();
    match args {
        Command::Lex { input_file } => lex_and_stop(&input_file),
        Command::Parse { input_file } => parse_and_stop(&input_file),
        Command::Run { input_file } => run(&input_file),
    }
}
