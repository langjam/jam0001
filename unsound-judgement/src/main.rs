#![allow(dead_code)]

mod parser;
mod raw_ast;
mod type_check;
mod vm;

const PRELUDE: &str = include_str!("prelude.nd");

fn main() {
	let program_text = {
		let mut text = PRELUDE.to_owned();
		let program_filename = std::env::args()
			.nth(1)
			.expect("The file passed to the interpreter should be in the first argument to the program");
		text.insert_str(
			text.len(),
			&std::fs::read_to_string(program_filename).unwrap(),
		);
		text
	};
	let program = parser::parse(&program_text);
	let result = vm::evaluate(program);
	match result {
		vm::Value::Tuple(v) if v.len() == 0 => {}
		_ => println!("result of program is {:?}", result),
	}
}
