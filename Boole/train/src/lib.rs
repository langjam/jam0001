use crate::ast::Program;
use crate::parse::parser::{Parser, ParseResult};

pub mod operations;
pub mod ast;
pub mod parse;
pub mod vm;
pub mod interface;
pub mod check;
pub mod wishes;

#[macro_use]
extern crate lazy_static;

extern crate strum;
#[macro_use]
extern crate strum_macros;

pub fn parse_and_check(input: &str) -> ParseResult<Program> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;

    check::check(&program, input)?;

    Ok(program)
}
