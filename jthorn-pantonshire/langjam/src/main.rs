mod interpreter;

use std::fs;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use anyhow::Context;
use clap::Clap;
use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct LangParser;

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
    let ast = LangParser::parse(Rule::program, &source)
        .expect("parse error"); //temporary shitty error message

    for pair in ast {
        match pair {
            _ => todo!(),
        }
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
