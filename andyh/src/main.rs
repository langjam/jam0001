mod evaluator;
mod tokenizer;
pub use evaluator::evaluate;
use std::{
    env,
    fs::File,
    io::{Read, Result, Write},
};
pub use tokenizer::*;

fn open_file(path: &str) -> Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    let (tokens, vars) = tokenize(&buf);
    Ok(evaluate(tokens, vars))
}

fn write_file(path: &str, output: &str) -> Result<()> {
    let mut file = File::create(&path)?;
    write!(file, "{}", output)
}

fn main() {
    let mut args = env::args();
    args.next(); // executable (possibly)
    for path in args {
        match open_file(&path) {
            Ok(output) => {
                if write_file(&path, &output).is_ok() {
                    println!("{}", output)
                } else {
                    eprint!("error whilst writing to {}", path)
                }
            }
            Err(_) => eprint!("error whilst opening {}", path),
        }
    }
}
