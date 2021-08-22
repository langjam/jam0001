use std::path::PathBuf;

use anyhow::Result;
use beatsaber::codegen::{self, CodegenOptions};
use beatsaber::{ast1, ast2, lexer};
use clap::{AppSettings, Clap};
use std::fs;

#[derive(Clap)]
#[clap(version = "0.1.0", author = "untitled")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Args {
    /// Input source file path.
    input: String,
    /// Output object file path.
    #[clap(short, default_value = "a.out")]
    output: PathBuf,
    /// Target triple
    #[clap(long)]
    target: Option<String>,
    /// Optimization level
    #[clap(short = 'O', default_value = "2")]
    optimization: u8,
    /// Generate position independent code
    #[clap(long)]
    pic: bool,
    /// C source files to compile and link
    #[clap(short = 'I')]
    include_c: Vec<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let src = fs::read_to_string(&args.input).unwrap();
    let lexer = lexer::lexer(&src, &args.input);
    let parser = ast1::parser(lexer);
    let ast2 = ast2::parse(parser);
    let options = CodegenOptions {
        output: args.output.as_path(),
        optimization: match args.optimization {
            0 => codegen::OptLevel::None,
            1 => codegen::OptLevel::Less,
            2 => codegen::OptLevel::Default,
            3 => codegen::OptLevel::Aggressive,
            _ => panic!("Invalid optimization level"),
        },
        pic: args.pic,
        target: args.target,
        include_c: args.include_c,
    };
    // dbg!(&ast2);
    codegen::Codegen::compile(ast2, options)?;

    Ok(())
}
