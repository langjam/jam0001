use std::collections::HashMap;
use std::iter::Peekable;

const SOURCE: &str = r#"
(print (! (== 0 5)))
"#;

// const SOURCE: &str = r#"
// (block
// 	(define x 0)
// 	(print "Initial value: " x)
// 	(set x 96)
// 	(print "Returned from block"
// 		(block
// 			(print "Something first in block")
// 			(print "Hello world from block" (+ (- (* 10 5) 10) (/ 4 2)))
// 			x)))
// "#;

#[derive(Debug)]
enum Token<'a> {
	OpenParen,
	CloseParen,
	IntegerLiteral(i64),
	Ident(&'a str),
	String(&'a str),
}

fn tokenize(source: &str) -> Vec<Token> {
	let mut current = &source[0..0];
	let mut current_start = 0;
	let mut output = Vec::new();

	fn finalize_current<'a>(
		source: &'a str,
		current: &mut &'a str,
		current_start: &mut usize,
		output: &mut Vec<Token<'a>>,
		index: usize,
	) {
		if !current.is_empty() {
			if let Ok(num) = current.parse() {
				output.push(Token::IntegerLiteral(num));
			} else {
				output.push(Token::Ident(current));
			}
			*current = &source[0..0];
		}

		*current_start = index + 1;
	}

	let mut iterator = source.bytes().enumerate();
	while let Some((index, byte)) = iterator.next() {
		match byte {
			b'(' | b')' => {
				finalize_current(source, &mut current, &mut current_start, &mut output, index);

				let token = match byte {
					b'(' => Token::OpenParen,
					b')' => Token::CloseParen,
					_ => unreachable!(),
				};
				output.push(token);
			}

			byte if byte.is_ascii_whitespace() => {
				finalize_current(source, &mut current, &mut current_start, &mut output, index);
			}

			b'"' => {
				finalize_current(source, &mut current, &mut current_start, &mut output, index);

				for (index, byte) in &mut iterator {
					match byte {
						b'"' => break,
						_ => current = &source[current_start..index + 1],
					}
				}

				output.push(Token::String(&current[0..current.len()]));
				current = &source[0..0];
				current_start = index + 1;
			}

			_ => {
				current = &source[current_start..index + 1];
			}
		}
	}

	output
}

#[derive(Debug)]
enum TreeNode<'a> {
	StringLiteral(&'a str),

	IntegerLiteral(i64),

	BooleanLiteral(bool),

	Block(Vec<TreeNode<'a>>),

	Define {
		name: &'a str,
		rhs: Box<TreeNode<'a>>,
	},

	Set {
		name: &'a str,
		rhs: Box<TreeNode<'a>>,
	},

	Read {
		name: &'a str,
	},

	Call {
		name: &'a str,
		args: Vec<TreeNode<'a>>,
	},
}

type TokenIterator<'a> = Peekable<std::slice::Iter<'a, Token<'a>>>;

fn parse_expression<'a>(tokens: &mut TokenIterator<'a>) -> TreeNode<'a> {
	let is_lone = !matches!(tokens.peek(), Some(Token::OpenParen));

	if !is_lone {
		parse_matching_parens(tokens)
	} else {
		match tokens.next() {
			Some(&Token::IntegerLiteral(num)) => TreeNode::IntegerLiteral(num),

			Some(&Token::String(string)) => TreeNode::StringLiteral(string),

			Some(&Token::Ident(text)) if text == "true" => TreeNode::BooleanLiteral(true),

			Some(&Token::Ident(text)) if text == "false" => TreeNode::BooleanLiteral(false),

			Some(Token::Ident(name)) => TreeNode::Read { name },

			None => panic!("No token for literal"),

			_ => panic!("Lone token not handled"),
		}
	}
}

fn parse_matching_parens<'a>(tokens: &mut TokenIterator<'a>) -> TreeNode<'a> {
	assert!(matches!(tokens.next(), Some(Token::OpenParen)));

	let ident = if let Some(Token::Ident(literal)) = tokens.next() {
		literal
	} else {
		panic!("First token after open paren must be ident");
	};

	match *ident {
		"define" => {
			let name = if let Some(Token::Ident(name)) = tokens.next() {
				name
			} else {
				panic!("Needed name to define");
			};

			let rhs = Box::new(parse_expression(tokens));

			assert!(matches!(tokens.next(), Some(Token::CloseParen)));

			TreeNode::Define { name, rhs }
		}

		"set" => {
			let name = if let Some(Token::Ident(name)) = tokens.next() {
				name
			} else {
				panic!("Needed name to set");
			};

			let rhs = Box::new(parse_expression(tokens));

			assert!(matches!(tokens.next(), Some(Token::CloseParen)));

			TreeNode::Set { name, rhs }
		}

		"block" => {
			let mut block = Vec::new();

			while let Some(peeked) = tokens.peek() {
				if !matches!(peeked, Token::CloseParen) {
					block.push(parse_expression(tokens));
				} else {
					break;
				}
			}

			TreeNode::Block(block)
		}

		ident => {
			let args = {
				let mut args = Vec::new();

				while let Some(peeked) = tokens.peek() {
					if !matches!(peeked, Token::CloseParen) {
						args.push(parse_expression(tokens));
					} else {
						tokens.next();
						break;
					}
				}

				args
			};

			TreeNode::Call { name: ident, args }
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
	None,
	Bool(bool),
	I64(i64),
	String(String),
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::None => write!(f, "None"),
			Value::Bool(lit) => write!(f, "{}", lit),
			Value::I64(num) => write!(f, "{}", num),
			Value::String(string) => write!(f, "{}", string),
		}
	}
}

#[derive(Debug)]
struct ScopeState {
	scopes: Vec<HashMap<String, Value>>,
}

impl ScopeState {
	fn new() -> ScopeState {
		ScopeState {
			scopes: vec![HashMap::new()],
		}
	}

	fn push_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}

	fn pop_scope(&mut self) {
		self.scopes.pop();
	}

	fn define(&mut self, name: &str, value: Value) -> Value {
		self.scopes
			.last_mut()
			.unwrap()
			.insert(name.to_string(), value.clone());

		value
	}

	fn set(&mut self, name: &str, value: Value) -> Value {
		for scope in self.scopes.iter_mut().rev() {
			if let Some(ptr) = scope.get_mut(name) {
				*ptr = value.clone();
				return value;
			}
		}

		panic!("Unknown variable {:?} to set", name);
	}

	fn read(&self, name: &str) -> Value {
		for scope in self.scopes.iter().rev() {
			if let Some(ptr) = scope.get(name) {
				return ptr.clone();
			}
		}

		panic!("Unknown variable {:?} to read", name);
	}
}

fn evaluate(state: &mut ScopeState, node: &TreeNode) -> Value {
	match node {
		&TreeNode::StringLiteral(string) => Value::String(string.to_string()),

		&TreeNode::IntegerLiteral(num) => Value::I64(num),

		&TreeNode::BooleanLiteral(lit) => Value::Bool(lit),

		TreeNode::Define { name, rhs } => {
			state.push_scope();
			let value = evaluate(state, rhs);
			state.pop_scope();

			state.define(name, value.clone());
			value
		}

		TreeNode::Set { name, rhs } => {
			state.push_scope();
			let value = evaluate(state, rhs);
			state.pop_scope();

			state.set(name, value.clone());
			value
		}

		TreeNode::Read { name } => state.read(name),

		TreeNode::Block(block) => {
			let mut result = Value::None;

			for entry in block {
				result = evaluate(state, entry);
			}

			result
		}

		TreeNode::Call { name, args } => {
			let args = {
				let mut evaluated = Vec::new();
				for arg in args {
					state.push_scope();
					evaluated.push(evaluate(state, arg));
					state.pop_scope();
				}
				evaluated
			};

			match *name {
				"+" => add_all(&args),
				"-" => sub_all(&args),
				"*" => mul_all(&args),
				"/" => div_all(&args),

				"==" => all_equal(&args),
				"!=" => all_not_equal(&args),
				"!" => invert_bool(&args),

				"print" => print_values(&args),

				_ => unimplemented!("TODO: Symbol lookup"),
			}
		}
	}
}

fn add_all(values: &[Value]) -> Value {
	let mut result = 0;

	for value in values {
		match value {
			Value::I64(num) => result += num,
			_ => panic!("Unsupported type of value passed to add"),
		}
	}

	Value::I64(result)
}

fn sub_all(values: &[Value]) -> Value {
	let mut result = None;

	for value in values {
		match value {
			Value::I64(num) => {
				if let Some(result) = &mut result {
					*result -= *num
				} else {
					result = Some(*num)
				}
			}

			_ => panic!("Unsupported type of value passed to sub"),
		}
	}

	Value::I64(result.unwrap_or(0))
}

fn mul_all(values: &[Value]) -> Value {
	let mut result = 1;

	for value in values {
		match value {
			Value::I64(num) => result *= num,
			_ => panic!("Unsupported type of value passed to mul"),
		}
	}

	Value::I64(result)
}

fn div_all(values: &[Value]) -> Value {
	let mut result = None;

	for value in values {
		match value {
			Value::I64(num) => {
				if let Some(result) = &mut result {
					*result /= *num
				} else {
					result = Some(*num)
				}
			}

			_ => panic!("Unsupported type of value passed to div"),
		}
	}

	Value::I64(result.unwrap_or(0))
}

fn all_equal(values: &[Value]) -> Value {
	for index in 1..values.len() {
		if values[index - 1] != values[index] {
			return Value::Bool(false);
		}
	}

	Value::Bool(true)
}

fn all_not_equal(values: &[Value]) -> Value {
	for index in 1..values.len() {
		if values[index - 1] == values[index] {
			return Value::Bool(false);
		}
	}

	Value::Bool(true)
}

fn invert_bool(values: &[Value]) -> Value {
	match values.last() {
		Some(Value::Bool(value)) => Value::Bool(!value),
		_ => Value::Bool(false),
	}
}

fn print_values(values: &[Value]) -> Value {
	let count = values.len();
	for (index, value) in values.iter().enumerate() {
		if index + 1 >= count {
			println!("{}", value);
		} else {
			print!("{} ", value);
		}
	}

	Value::None
}

fn main() {
	println!("Tokenizing and parsing source: `{}`", SOURCE);
	let tokens = tokenize(SOURCE);
	let tree = parse_matching_parens(&mut tokens.iter().peekable());
	println!("==== Evaluating ====");
	let mut state = ScopeState::new();
	evaluate(&mut state, &tree);
}
