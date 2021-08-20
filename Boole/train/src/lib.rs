pub mod operations;
pub mod ast;
pub mod parse;
pub mod vm;
pub mod interface;

#[macro_use]
extern crate lazy_static;

extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::ast::Program;
use crate::parse::parser::{Parser, ParseResult};

pub fn compile_and_check(input: &str) -> ParseResult<Program> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;


    Ok(program)
}
