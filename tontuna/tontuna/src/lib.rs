#![allow(dead_code, unused_variables)]

mod ast;
mod eval;
mod lexer;
mod parser;
mod pos;

use std::{io::Write, rc::Rc};

pub use crate::pos::{Pos, Span};

#[derive(Debug)]
pub struct Ast {
    source: Rc<str>,
    program: ast::Program,
}

pub fn parse(source: &str) -> Result<Ast, Error> {
    Ok(Ast {
        source: source.into(),
        program: crate::parser::parse(&source)?,
    })
}

pub fn eval(ast: &Ast, output: Box<dyn Write>) -> Result<(), Error> {
    let mut evaluator = eval::Evaluator::new(
        ast.source.clone(),
        Some(&ast.program),
        output,
    );
    evaluator.run_program(&ast.program)
        .map_err(|e| Error { span: e.span.unwrap(), message: e.message })
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub message: String,
}
