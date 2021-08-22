#![feature(exclusive_range_pattern)]
#![feature(map_try_insert)]
#![feature(in_band_lifetimes)]

use crate::interp::builtin_comment;
use crate::parser::{find_comments_mut, Expr, Program};
use anyhow::anyhow;
use interp::Interpreter;

mod interp;
mod parser;
mod reassemble;

pub fn run(code: &str) -> anyhow::Result<String> {
    let mut program = parser::parser::program(code)?;
    let mut interp = Interpreter::new();
    for (_, comment) in find_comments_mut(&mut program)? {
        interp.add_comment(comment)?;
    }

    let block = Expr::Block(program.block.clone());
    interp.interp(&block)?;

    replace_comments_in_source_code(&mut program, &mut interp)?;

    Ok(reassemble::output_code(&program))
}

pub fn replace_comments_in_source_code(
    mut program: &mut Program,
    interp: &mut Interpreter,
) -> anyhow::Result<()> {
    let mut comments = find_comments_mut(&mut program)?;
    for (name, body) in interp.comments() {
        let code_comment = comments
            .get_mut(name)
            .ok_or_else(|| anyhow!("original code didn't contain comment {}", name))?;
        code_comment.body = if let Some(builtin) = builtin_comment(interp, name) {
            builtin.to_string()
        } else {
            body.to_string()
        };
    }
    Ok(())
}
