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