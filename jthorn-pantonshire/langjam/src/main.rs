mod error;
mod parser;
mod value;
mod interpreter;
mod stdlib;

use std::fs;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use anyhow::{anyhow, Context};
use clap::Clap;

#[derive(Clap)]
struct Opts {
    /// The program file to run
    path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();

    match opts.path {
        Some(source_path) => {
            let source = fs::read_to_string(&source_path)
                .with_context(|| format!(r#"failed to read source file "{}""#, source_path.to_string_lossy()))?;
            drop(source_path);
            run_source(source)
        },

        None => run_cli(),
    }
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

fn run_cli() -> anyhow::Result<()> {
    fn read_stdin_line() -> io::Result<Option<String>> {
        io::stdin()
            .lock()
            .lines()
            .next()
            .transpose()
    }

    loop {
        {
            let stdout = io::stdout();
            let mut stdout_lock = stdout.lock();
            write!(&mut stdout_lock, ">> ").context("failed to write to stdout")?;
            stdout_lock.flush().context("failed to flush stdout")?;
        }

        match read_stdin_line().context("failed to read from stdin")? {
            None => return Ok(()),
            Some(ref line) => match line.trim() {
                ":q" => return Ok(()),
                line => println!("{}", line), //temporary
            },
        }
    }
}
