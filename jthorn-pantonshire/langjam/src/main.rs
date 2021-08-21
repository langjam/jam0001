mod error;
mod parser;
mod value;
mod interpreter;

use std::fs;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use anyhow::Context;
use clap::Clap;
use pest::Parser;

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
        Err(err) => todo!("error reporting"),
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
