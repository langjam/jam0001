mod error;
mod parser;
mod value;
mod interpreter;
mod stdlib;

use std::fs;
use std::path::PathBuf;

use anyhow::{anyhow, Context};
use clap::Clap;

#[derive(Clap)]
struct Opts {
    /// The program file to run
    path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();
    let source = fs::read_to_string(&opts.path)
        .with_context(|| format!(r#"failed to read source file "{}""#, opts.path.to_string_lossy()))?;
    drop(opts.path);
    run_source(source)
}

fn run_source(source: String) -> anyhow::Result<()> {
    let ast = parser::parse(&source)?;
    let interpreter = interpreter::Interpreter::new(ast);
    let res = interpreter.exec_main();

    match res {
        Ok(value::Value::Void) => (),
        Ok(val) => println!("> {}", val),
        Err(err) => {
            eprintln!("error at line {}", err.span.line_no(&source));

            let (err_type, err_msg) = match err.cause {
                error::RuntimeErrorCause::MissingVariable(msg) => ("variable not found", msg),
                error::RuntimeErrorCause::MissingFunction(msg) => ("function not found", msg),
                error::RuntimeErrorCause::TypeError(msg) => ("type", msg),
                error::RuntimeErrorCause::Immutable(msg) => ("mutability", msg),
                error::RuntimeErrorCause::OutOfBoundsError(msg) => ("out of bounds", msg),
            };

            eprintln!("{} error: {}", err_type, err_msg);

            let problem_code = &source[err.span.start..err.span.end];

            eprintln!("{}", problem_code);

            let mut underline = String::with_capacity(problem_code.len());
            for _ in 0..problem_code.len() {
                underline.push('^');
            }

            eprintln!("{}", underline);

            return Err(anyhow!("program exited with error :("));
        },
    }

    Ok(())
}
