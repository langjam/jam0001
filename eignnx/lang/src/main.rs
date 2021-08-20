mod ast;
mod parser;

fn main() {
    let path = std::env::args()
        .skip(1)
        .next()
        .expect("You must provide a source code file to run!");
    let tm = match parser::parse_from_file(path) {
        Ok(tm) => tm,
        Err(e) => {
            eprintln!("Parse Error:\n{}", e);
            std::process::exit(1);
        }
    };
    dbg!(tm);
}

#[test]
fn test_parse() {
    let ast = parser::parse(r#"  [fn asdf:Int->"balkjasdf"]"#).unwrap();
    dbg!(ast);
}

#[test]
fn test_parse_from_file() {
    let ast = parser::parse_from_file(r#"ex\ex1.txt"#).unwrap();
    dbg!(ast);
}
