// use std::collections::HashMap;
use std::iter::Peekable;

const SOURCE: &str = r#"(print "Hello world!" (+ (- (* 10 5) 10) (/ 4 2)))"#;

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

	#[allow(clippy::match_single_binding)]
	match ident {
		//TODO: Match `block`
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
	I64(i64),
	String(String),
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::None => write!(f, "None"),
			Value::I64(num) => write!(f, "{}", num),
			Value::String(string) => write!(f, "{}", string),
		}
	}
}

// struct ScopeState<'a> {
// 	parent: Option<&'a mut ScopeState<'a>>,
// 	scope: HashMap<String, Value>,
// }

fn evaluate(node: &TreeNode) -> Value {
	match node {
		&TreeNode::StringLiteral(string) => Value::String(string.to_string()),

		&TreeNode::IntegerLiteral(num) => Value::I64(num),

		TreeNode::Call { name, args } => {
			let args = args.iter().map(|arg| evaluate(arg)).collect::<Vec<Value>>();

			match *name {
				"+" => add_all(&args),
				"-" => sub_all(&args),
				"*" => mul_all(&args),
				"/" => div_all(&args),

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
	println!("Preparing to run source: `{}`", SOURCE);
	let tokens = tokenize(SOURCE);
	let tree = parse_matching_parens(&mut tokens.iter().peekable());
	evaluate(&tree);
}
