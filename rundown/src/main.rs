#[macro_use]
extern crate pest_derive;

use std::fs;
use std::path::PathBuf;

use ::markdown::{generate_markdown, Block};
use anyhow::{anyhow, Result};
use structopt::StructOpt;

mod ast;
mod builtins;
mod eval;
mod markdown;

use crate::builtins::builtins;
use crate::eval::{Context, StatementResult};
use crate::markdown::{construct_index, print_markdown};

const RUNDOWN_CODE_BLOCK_SYNTAX: &str = "rundown";

#[derive(Debug, StructOpt)]
#[structopt(name = "rundown", about = "Run your markdown adventure!")]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

fn main() -> Result<()> {
    let opt = Opt::from_args();
    let input = fs::read_to_string(&opt.input)?;
    let section_index = construct_index(&input);

    let mut context = Context::new(builtins());

    let mut pc = 0;
    'outer: while let Some((_name, section)) = section_index.get_index(pc) {
        for block in section {
            match block {
                Block::CodeBlock(Some(syntax), content) if syntax == RUNDOWN_CODE_BLOCK_SYNTAX => {
                    let statements = ast::parse(content)?;
                    let res = context.eval(&statements)?;

                    if let StatementResult::Goto(label) = res {
                        pc = section_index
                            .get_index_of(&label)
                            .ok_or_else(|| anyhow!("Tried to goto section that does not exist"))?;
                        continue 'outer;
                    }
                }
                _ => {
                    let content = generate_markdown(vec![block.clone()]);
                    print_markdown(&content)?;
                }
            }
        }

        pc += 1;
    }

    Ok(())
}
