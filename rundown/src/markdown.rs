use std::env;
use std::io::stdout;

use anyhow::Result;
use indexmap::IndexMap;
use lazy_static::lazy_static;
use markdown::{Block, Span};
use mdcat::{Environment, ResourceAccess, Settings, TerminalCapabilities, TerminalSize};
use pulldown_cmark::{Options, Parser as MdParser};
use slugify::slugify;
use syntect::parsing::SyntaxSet;

const INTRO_SECTION: &str = "intro";

lazy_static! {
    static ref MDCAT_SETTINGS: Settings = Settings {
        terminal_capabilities: TerminalCapabilities::detect(),
        terminal_size: TerminalSize::from_terminal().unwrap(),
        resource_access: ResourceAccess::LocalOnly,
        syntax_set: SyntaxSet::load_defaults_newlines(),
    };
    static ref MDCAT_ENV: Environment =
        Environment::for_local_directory(&env::current_dir().unwrap()).unwrap();
}

fn spans_to_string(spans: &[Span]) -> String {
    spans
        .iter()
        .map(|span| match span {
            Span::Text(ref s) | Span::Code(ref s) | Span::Image(ref s, _, _) => s.to_owned(),
            Span::Literal(c) => c.to_string(),
            Span::Link(s, _, _) | Span::RefLink(s, _, _) | Span::Emphasis(s) | Span::Strong(s) => {
                spans_to_string(s)
            }
            _ => "".to_owned(),
        })
        .collect::<Vec<String>>()
        .join("")
}

pub fn construct_index(content: &str) -> IndexMap<String, Vec<Block>> {
    let mut ret: IndexMap<String, Vec<Block>> = IndexMap::new();

    let mut current_section = ret.entry(INTRO_SECTION.to_owned()).or_default();
    for block in markdown::tokenize(content) {
        match block {
            Block::Header(spans, _) => {
                let name = slugify!(&spans_to_string(&spans).to_lowercase());
                current_section = ret.entry(name).or_default();
            }
            _ => {
                current_section.push(block.clone());
            }
        }
    }

    ret
}

pub fn print_markdown(content: &str) -> Result<()> {
    // TODO: Calculate this once?
    let parser = MdParser::new_ext(
        content,
        Options::ENABLE_TASKLISTS | Options::ENABLE_STRIKETHROUGH,
    );

    let stdout = stdout();
    let mut handle = stdout.lock();
    mdcat::push_tty(&MDCAT_SETTINGS, &MDCAT_ENV, &mut handle, parser)?;

    Ok(())
}
