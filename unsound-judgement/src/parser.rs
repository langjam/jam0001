use lalrpop_util::lalrpop_mod;
use crate::raw_ast::*;

lalrpop_mod!(grammar, "/parser/grammar.rs");

pub fn parse(s: &str) -> Program {
	grammar::ProgramParser::new()
		.parse(s)
		.unwrap()
}
