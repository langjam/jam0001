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
                dank::data::Value::NativeFn(dank::data::Ptr::new(dank::data::NativeFn {
                    name: "clock".into(),
                    arity: 0,
                    func: Box::new(|_args| {
                        dank::data::Value::Num(
                            std::time::SystemTime::now()
                                .duration_since(std::time::SystemTime::UNIX_EPOCH)
                                .unwrap()
                                .as_secs() as f64,
                        )
                        .into()
                    }),
                })),
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
