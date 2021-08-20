use std::iter::Peekable;

const SOURCE: &str = r#"(print "hello world")"#;

#[derive(Debug)]
enum Token<'a> {
	OpenParen,
	CloseParen,
	Ident(&'a str),
	String(&'a str),
}

fn tokenize(source: &str) -> Vec<Token> {
	let mut current = &source[0..0];
	let mut current_start = 0;
	let mut output = Vec::new();

	let mut iterator = source.bytes().enumerate();
	while let Some((index, byte)) = iterator.next() {
		match byte {
			b'(' | b')' => {
				if !current.is_empty() {
					output.push(Token::Ident(current));
					current = &source[0..0];
				}

				current_start = index + 1;

				let token = match byte {
					b'(' => Token::OpenParen,
					b')' => Token::CloseParen,
					_ => unreachable!(),
				};
				output.push(token);
			}

			byte if byte.is_ascii_whitespace() => {
				if !current.is_empty() {
					output.push(Token::Ident(current));
					current = &source[0..0];
				}

				current_start = index + 1;
			}

			b'"' => {
				if !current.is_empty() {
					output.push(Token::Ident(current));
					current = &source[0..0];
					current_start = index + 1;
				}

				for (index, byte) in &mut iterator {
					match byte {
						b'"' => break,
						_ => current = &source[current_start..index + 1],
					}
				}

				output.push(Token::String(&current[1..current.len()]));
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
	Call {
		name: &'a str,
		args: Vec<TreeNode<'a>>,
	},
}

type TokenIterator<'a> = Peekable<std::slice::Iter<'a, Token<'a>>>;

fn parse_expression<'a>(tokens: &mut TokenIterator<'a>) -> TreeNode<'a> {
	let is_lone = matches!(tokens.peek(), Some(Token::OpenParen));

	if is_lone {
		parse_matching_parens(tokens)
	} else {
		match tokens.next() {
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
		//TODO: Match builtin ident names
		ident => {
			let args = {
				let mut args = Vec::new();

				while let Some(peeked) = tokens.peek() {
					if !matches!(peeked, Token::CloseParen) {
						args.push(parse_expression(tokens));
					} else {
						tokens.next();
					}
				}

				args
			};

			TreeNode::Call { name: ident, args }
		}
	}
}

fn main() {
	let tokens = tokenize(SOURCE);
	let tree = parse_matching_parens(&mut tokens.iter().peekable());
	println!("{:#?}", tree);
}
