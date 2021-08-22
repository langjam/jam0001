use std::io::Write;

use ast2str::AstToStr;
use liner::parser::grammar;

use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(version = "1.0")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(name = "file.ln")]
    input: String,
    #[clap(short = 'a', long = "print-ast")]
    print_ast: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts: Opts = Opts::parse();

    let source = std::fs::read_to_string(&opts.input)?;
    let arena = bumpalo::Bump::default();

    // TODO: report the error properly
    let mut ast = grammar::file(&source)?;

    if opts.print_ast {
        println!("{}", ast.ast_to_str());
    }

    let mut eval = liner::eval::Evaluator::with_env(
        {
            let mut env = liner::env::Env::new();
            env.add(
                "clock".into(),
                liner::data::NativeFn::create("clock", 0, |_args| {
                    liner::data::Value::Num(
                        std::time::SystemTime::now()
                            .duration_since(std::time::SystemTime::UNIX_EPOCH)
                            .unwrap()
                            .as_secs() as f64,
                    )
                    .into()
                })
                .into(),
            );
            env.add(
                "input".into(),
                liner::data::NativeFn::create("input", 1, |mut args| {
                    let prompt = args.pop().unwrap().to_string();
                    print!("{}", prompt);
                    std::io::stdout().flush().unwrap();
                    let mut buf = String::new();
                    std::io::stdin()
                        .read_line(&mut buf)
                        .expect("Couldn't read from STDIN");
                    liner::data::Value::Str(buf.trim().to_owned().into()).into()
                })
                .into(),
            );
            env.add(
                "dbg".into(),
                liner::data::NativeFn::create("debug", 1, |mut args| {
                    println!("{}", args.pop().unwrap().to_string());
                    liner::data::Value::Null.into()
                })
                .into(),
            );

            let arena = &arena;
            env.add(
                "fmt".into(),
                liner::data::NativeFn::create("fmt", -1, move |args| {
                    liner::ast_proxy::fmt_impl(arena, args)
                })
                .into(),
            );
            env
        },
        &source,
        &arena,
    );
    match eval.eval(&mut ast) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{:?}", e);
        }
    }

    Ok(())
}
