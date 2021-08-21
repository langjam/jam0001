mod ast;
pub mod eval;
mod parser;
pub use ast::AST;
pub use parser::parse_input;
