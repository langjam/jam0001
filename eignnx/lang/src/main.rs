use eval::Eval;

use crate::check::Check;

mod ast;
mod check;
mod eval;
mod parser;
mod tcx;

fn main() {
    let path = std::env::args()
        .skip(1)
        .next()
        .expect("You must provide a source code file to run!");

    // Parsing.
    let tm = match parser::parse_from_file(path) {
        Ok(tm) => tm,
        Err(e) => {
            eprintln!("Parse Error:\n{}", e);
            std::process::exit(1);
        }
    };

    // Type checking.
    let mut check = Check::new();
    let ty = match check.infer(&tm) {
        Ok(ty) => ty,
        Err(err) => {
            eprintln!("Type Error:\n{}", err);
            std::process::exit(1);
        }
    };

    // Evaluating.
    let mut eval = Eval::new();
    let val = eval.eval(&tm);
    println!(">>> {} : {}", val, ty);
}

#[test]
fn test_parse_from_file() {
    let ast = parser::parse_from_file(r#"ex\ex1.txt"#).unwrap();
    dbg!(ast);
}
