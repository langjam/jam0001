#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod model;
mod interpreter;

fn main() {
    println!("{:?}", grammar::ProgramParser::new().parse("returns nothing 10 + 10 * 10 \"boy howdy\"").unwrap());
}
