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
					current_start = index + 1;
				}

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
					current_start = index + 1;
				}
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

fn main() {
	println!("{:?}", tokenize(SOURCE));
}
