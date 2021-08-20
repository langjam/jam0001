use crate::ast::Program;
use crate::parse::parser::ParseResult;

mod check_names;

pub fn check(program: &Program) -> ParseResult<()> {
    check_names::check_names(program)?;
    Ok(())
}