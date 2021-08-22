use std::{fs, path::Path};

use args::Args;

use suslang_lib::{
    eval::{remove_refs, Evaluator, Value},
    *,
};
mod args;

pub type Result<T> = anyhow::Result<T>;

fn main() -> Result<()> {
    let args = Args::read();

    let build_path = Path::new(&args.build_script)
        .to_str()
        .ok_or(anyhow::anyhow!("Invalid build script path"))?;
    let build = fs::read_to_string(&args.build_script)?;
    let build = build.as_str();
    println!("Parsing Build File");
    let mut build_ast = parse_input(build, build_path)?.to_value();
    remove_refs(&mut build_ast);

    // insert synthetic call to `build`
    let call = ast_obj! { "ASTFnCall";
        "name" => Value::String("build".into()),
        "args" => Value::List{
            elems: vec![
                ast_obj!{ "ASTListLiteral";
                    "elements" => Value::List {
                        elems:  args.args.into_iter().map(|arg| ast_obj!{ "ASTStringLiteral";
                                    "text" => Value::String(arg)
                                }).collect()
                    }
                }
            ]
        }
    };
    match &mut build_ast {
        Value::Object(o) => {
            let elems = o.get_field_mut("elements").unwrap();
            match elems {
                Value::List { elems } => elems.push(call),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }

    println!("Executing Build File");
    let mut eval = Evaluator::new();
    eval.eval_ast(&mut build_ast)
        .map_err(|s| anyhow::anyhow!(s))?;

    Ok(())
}
