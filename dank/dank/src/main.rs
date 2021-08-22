use std::io::Write;

use ast2str::AstToStr;
use dank::parser::dank as parser;

use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(version = "1.0")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(name = "file.dk")]
    input: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts: Opts = Opts::parse();

    let source = std::fs::read_to_string(&opts.input)?;
    let arena = bumpalo::Bump::default();

    // TODO: report the error properly
    let mut ast = parser::file(&source)?;

    println!("{}", ast.ast_to_str());

    let mut eval = dank::eval::Evaluator::with_env(
        {
            let mut env = dank::env::Env::new();
            env.add(
                "clock".into(),
                dank::data::NativeFn::create("clock", 0, |_args| {
                    dank::data::Value::Num(
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
                dank::data::NativeFn::create("input", 1, |mut args| {
                    let prompt = args.pop().unwrap().to_string();
                    print!("{}", prompt);
                    std::io::stdout().flush().unwrap();
                    let mut buf = String::new();
                    std::io::stdin()
                        .read_line(&mut buf)
                        .expect("Couldn't read from STDIN");
                    dank::data::Value::Str(buf.trim().to_owned().into()).into()
                })
                .into(),
            );
            env.add(
                "dbg".into(),
                dank::data::NativeFn::create("debug", 1, |mut args| {
                    println!("{}", args.pop().unwrap().to_string());
                    dank::data::Value::Null.into()
                })
                .into(),
            );
            env
        },
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
