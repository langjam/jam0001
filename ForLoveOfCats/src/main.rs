use std::collections::HashMap;
use std::io::Write;
use std::iter::Peekable;

type Result<T> = std::result::Result<T, ()>;

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

	finalize_current(source, &mut current, &mut current_start, &mut output, 0);

	output
}

#[derive(Debug, Clone, PartialEq)]
enum TreeNode {
	Unit,

	StringLiteral(String),

	IntegerLiteral(i64),

	BooleanLiteral(bool),

	LambdaLiteral(Box<Lambda>),

	Block(Vec<TreeNode>),

	While {
		expr: Box<TreeNode>,
		body: Box<TreeNode>,
	},

	If {
		expr: Box<TreeNode>,
		true_body: Box<TreeNode>,
		false_body: Box<TreeNode>,
	},

	Define {
		name: String,
		rhs: Box<TreeNode>,
	},

	Set {
		name: String,
		rhs: Box<TreeNode>,
	},

	Assign {
		name: String,
		index_expr: Box<TreeNode>,
		expr: Box<TreeNode>,
	},

	Push {
		name: String,
		expr: Box<TreeNode>,
	},

	Pop {
		name: String,
	},

	TypeOf {
		expr: Box<TreeNode>,
	},

	Read {
		name: String,
	},

	Call {
		name: String,
		args: Vec<TreeNode>,
	},
}

type TokenIterator<'a> = Peekable<std::slice::Iter<'a, Token<'a>>>;

macro_rules! parse_error {
	( $($arg:tt)* ) => {{
		println!($($arg)*);
		Err(())
	}}
}

macro_rules! expect_token {
	($iterator:ident, $pattern:pat) => {
		match $iterator.next() {
			Some($pattern) => Ok(()),

			None => parse_error!("Expected token {}", stringify!($pattern)),

			_ => parse_error!("Unexpected EOF, expected token {}", stringify!($pattern)),
		}
	};
}

fn parse_expression(tokens: &mut TokenIterator) -> Result<TreeNode> {
	let is_lone = !matches!(tokens.peek(), Some(Token::OpenParen));

	if !is_lone {
		parse_matching_parens(tokens)
	} else {
		Ok(match tokens.next() {
			Some(&Token::IntegerLiteral(num)) => TreeNode::IntegerLiteral(num),

			Some(&Token::String(string)) => TreeNode::StringLiteral(string.to_string()),

			Some(&Token::Ident(text)) if text == "true" => TreeNode::BooleanLiteral(true),

			Some(&Token::Ident(text)) if text == "false" => TreeNode::BooleanLiteral(false),

			Some(&Token::Ident(name)) => TreeNode::Read {
				name: name.to_string(),
			},

			None => parse_error!("No token for literal")?,

			token => parse_error!("Lone token not handled: {:?}", token)?,
		})
	}
}

fn parse_parameters(tokens: &mut TokenIterator) -> Result<Vec<String>> {
	expect_token!(tokens, Token::OpenParen)?;

	let mut parameters = Vec::new();

	while let Some(peeked) = tokens.peek() {
		if !matches!(peeked, Token::CloseParen) {
			let token = tokens.next().unwrap();

			let name = match token {
				Token::Ident(name) => name.to_string(),
				_ => parse_error!("All parameter tokens must be identifiers")?,
			};

			parameters.push(name);
		} else {
			break;
		}
	}

	expect_token!(tokens, Token::CloseParen)?;

	Ok(parameters)
}

fn parse_matching_parens(tokens: &mut TokenIterator) -> Result<TreeNode> {
	expect_token!(tokens, Token::OpenParen)?;

	let ident = match tokens.next() {
		Some(Token::Ident(ident)) => ident,
		Some(Token::CloseParen) => return Ok(TreeNode::Unit),
		_ => parse_error!("Expected close paren")?,
	};

	Ok(match *ident {
		"define" => {
			let name = if let Some(Token::Ident(name)) = tokens.next() {
				name
			} else {
				return parse_error!("Needed name to define");
			};

			let rhs = Box::new(parse_expression(tokens)?);

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::Define {
				name: name.to_string(),
				rhs,
			}
		}

		"set" => {
			let name = if let Some(Token::Ident(name)) = tokens.next() {
				name
			} else {
				return parse_error!("Needed name to set");
			};

			let rhs = Box::new(parse_expression(tokens)?);

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::Set {
				name: name.to_string(),
				rhs,
			}
		}

		"assign" => {
			let name = if let Some(Token::Ident(name)) = tokens.next() {
				name
			} else {
				return parse_error!("Needed name to assign");
			};

			let index_expr = Box::new(parse_expression(tokens)?);
			let expr = Box::new(parse_expression(tokens)?);

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::Assign {
				name: name.to_string(),
				index_expr,
				expr,
			}
		}

		"push" => {
			let name = if let Some(Token::Ident(name)) = tokens.next() {
				name
			} else {
				return parse_error!("Needed name to push to");
			};

			let expr = Box::new(parse_expression(tokens)?);

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::Push {
				name: name.to_string(),
				expr,
			}
		}

		"pop" => {
			let name = if let Some(Token::Ident(name)) = tokens.next() {
				name
			} else {
				return parse_error!("Needed name to pop from");
			};

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::Pop {
				name: name.to_string(),
			}
		}

		"type-of" => {
			let expr = Box::new(parse_expression(tokens)?);

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::TypeOf { expr }
		}

		"block" => {
			let mut block = Vec::new();

			while let Some(peeked) = tokens.peek() {
				if !matches!(peeked, Token::CloseParen) {
					block.push(parse_expression(tokens)?);
				} else {
					break;
				}
			}

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::Block(block)
		}

		"while" => {
			let expr = Box::new(parse_expression(tokens)?);
			let body = Box::new(parse_expression(tokens)?);

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::While { expr, body }
		}

		"if" => {
			let expr = Box::new(parse_expression(tokens)?);
			let true_body = Box::new(parse_expression(tokens)?);
			let false_body = Box::new(parse_expression(tokens)?);

			expect_token!(tokens, Token::CloseParen)?;

			TreeNode::If {
				expr,
				true_body,
				false_body,
			}
		}

		"lambda" => {
			let parameters = parse_parameters(tokens)?;
			let body = parse_expression(tokens)?;

			expect_token!(tokens, Token::CloseParen)?;

			let lambda = Lambda { parameters, body };

			TreeNode::LambdaLiteral(Box::new(lambda))
		}

		ident => {
			let args = {
				let mut args = Vec::new();

				while let Some(peeked) = tokens.peek() {
					if !matches!(peeked, Token::CloseParen) {
						args.push(parse_expression(tokens)?);
					} else {
						tokens.next();
						break;
					}
				}

				args
			};

			TreeNode::Call {
				name: ident.to_string(),
				args,
			}
		}
	})
}

macro_rules! runtime_error {
	($state:ident, $($arg:tt)* ) => {{
		print!("Runtime panic: ");
		println!($($arg)*);

		if !$state.in_repl {
			$state.in_repl = true;
			repl($state);
			$state.in_repl = false;
		}

		Value::None
	}}
}

#[derive(Debug, Clone, PartialEq)]
struct Lambda {
	parameters: Vec<String>,
	body: TreeNode,
}

impl Lambda {
	fn call(&self, state: &mut ScopeState, args: &[Value]) -> Value {
		if args.len() != self.parameters.len() {
			runtime_error!(state, "Invalid number of arguments");
		}

		state.push_scope();

		for (parameter, arg) in self.parameters.iter().zip(args) {
			state.define(parameter, arg.clone());
		}

		let value = evaluate(state, &self.body);

		state.pop_scope();

		value
	}
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
	None,
	Bool(bool),
	I64(i64),
	List(Vec<Value>),
	String(String),
	Lambda(Lambda),
}

impl Value {
	fn type_int(&self) -> i64 {
		match self {
			Value::None => 0,
			Value::Bool(_) => 1,
			Value::I64(_) => 2,
			Value::List(_) => 3,
			Value::String(_) => 4,
			Value::Lambda(_) => 5,
		}
	}
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::None => write!(f, "None"),

			Value::Bool(lit) => write!(f, "{}", lit),

			Value::I64(num) => write!(f, "{}", num),

			Value::List(list) => {
				write!(f, "[")?;

				let count = list.len();
				for (index, entry) in list.iter().enumerate() {
					if index + 1 >= count {
						write!(f, "{}", entry)?;
					} else {
						write!(f, "{}, ", &entry)?;
					}
				}

				write!(f, "]")
			}

			Value::String(string) => write!(f, "{}", string),

			Value::Lambda { .. } => write!(f, "`Lambda Object`"),
		}
	}
}

#[derive(Debug)]
struct Scope {
	symbols: HashMap<String, Value>,
	comments: Vec<Vec<Value>>,
}

impl Scope {
	fn new() -> Scope {
		Scope {
			symbols: HashMap::new(),
			comments: Vec::new(),
		}
	}
}

macro_rules! map {
	[ $($name:literal = $value:expr ,)* ] => {{
		let mut map = HashMap::new();
		$(map.insert($name.to_string(), $value);)*
		map
	}}
}

#[derive(Debug)]
struct ScopeState {
	scopes: Vec<Scope>,
	in_repl: bool,
}

impl ScopeState {
	fn new() -> ScopeState {
		ScopeState {
			scopes: vec![Scope {
				symbols: map![
					"type-none" = Value::I64(0),
					"type-bool" = Value::I64(1),
					"type-i64" = Value::I64(2),
					"type-list" = Value::I64(3),
					"type-string" = Value::I64(4),
					"type-lambda" = Value::I64(5),
				],
				comments: Vec::new(),
			}],
			in_repl: false,
		}
	}

	fn push_scope(&mut self) {
		self.scopes.push(Scope::new());
	}

	fn pop_scope(&mut self) {
		self.scopes.pop();
	}

	fn insert_comment(&mut self, values: Vec<Value>) {
		self.scopes.last_mut().unwrap().comments.push(values);
	}

	fn define(&mut self, name: &str, value: Value) -> Value {
		self.scopes
			.last_mut()
			.unwrap()
			.symbols
			.insert(name.to_string(), value.clone());

		value
	}

	fn set(&mut self, name: &str, value: Value) -> Value {
		for scope in self.scopes.iter_mut().rev() {
			if let Some(ptr) = scope.symbols.get_mut(name) {
				*ptr = value.clone();
				return value;
			}
		}

		runtime_error!(self, "Unknown variable {:?} to set", name)
	}

	fn read(&mut self, name: &str) -> Value {
		for scope in self.scopes.iter().rev() {
			if let Some(ptr) = scope.symbols.get(name) {
				return ptr.clone();
			}
		}

		runtime_error!(self, "Unknown variable {:?} to read", name)
	}
}

fn evaluate(state: &mut ScopeState, node: &TreeNode) -> Value {
	match node {
		TreeNode::Unit => Value::None,

		TreeNode::StringLiteral(string) => Value::String(string.to_string()),

		&TreeNode::IntegerLiteral(num) => Value::I64(num),

		&TreeNode::BooleanLiteral(lit) => Value::Bool(lit),

		TreeNode::LambdaLiteral(lambda) => Value::Lambda(Lambda::clone(lambda)),

		TreeNode::Define { name, rhs } => {
			let value = evaluate(state, rhs);
			state.define(name, value.clone());
			value
		}

		TreeNode::Set { name, rhs } => {
			let value = evaluate(state, rhs);
			state.set(name, value.clone());
			value
		}

		TreeNode::Assign {
			name,
			index_expr,
			expr,
		} => {
			let index = evaluate(state, index_expr);
			let index = unwrap_integer(state, &index);
			let index = unwrap_usize(state, index);

			let value = evaluate(state, expr);

			let mut target = state.read(name);
			match &mut target {
				Value::List(list) => {
					if index >= list.len() {
						runtime_error!(state, "Index {} out of bounds to assign", index);
					} else {
						list[index] = value.clone();
					}
				}
				_ => {
					runtime_error!(state, "Cannot assign to value of non-list type");
				}
			};
			state.set(name, target);

			value
		}

		TreeNode::Push { name, expr } => {
			let value = evaluate(state, expr);

			let mut target = state.read(name);
			match &mut target {
				Value::List(list) => list.push(value.clone()),
				_ => {
					runtime_error!(state, "Cannot push to value of non-list type");
				}
			};
			state.set(name, target);

			value
		}

		TreeNode::Pop { name } => {
			let mut target = state.read(name);
			let value = match &mut target {
				Value::List(list) => match list.pop() {
					Some(value) => value,
					None => runtime_error!(state, "Cannot pop from empty list"),
				},
				_ => runtime_error!(state, "Cannot push to value of non-list type"),
			};
			state.set(name, target);

			value
		}

		TreeNode::TypeOf { expr } => {
			let value = evaluate(state, expr);
			Value::I64(value.type_int())
		}

		TreeNode::Read { name } => state.read(name),

		TreeNode::Block(block) => {
			let mut result = Value::None;

			for entry in block {
				result = evaluate(state, entry);
			}

			result
		}

		TreeNode::While { expr, body } => {
			let mut result = Value::None;

			loop {
				let expr_result = evaluate(state, expr);
				let should_loop = !matches!(expr_result, Value::Bool(false));
				if !should_loop {
					break;
				}

				state.push_scope();
				result = evaluate(state, body);
				state.pop_scope();
			}

			result
		}

		TreeNode::If {
			expr,
			true_body,
			false_body,
		} => {
			let expr_result = evaluate(state, expr);
			let truthy = !matches!(expr_result, Value::Bool(false));

			if truthy {
				evaluate(state, true_body)
			} else {
				evaluate(state, false_body)
			}
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

			match name.as_str() {
				"#" => insert_comment(state, &args),

				"+" => add_all(state, &args),
				"-" => sub_all(state, &args),
				"*" => mul_all(state, &args),
				"/" => div_all(state, &args),

				"==" => all_equal(state, &args),
				"!=" => all_not_equal(state, &args),
				"!" => invert_bool(state, &args),

				"<" => less(state, &args),
				"<=" => less_equal(state, &args),
				">" => greater(state, &args),
				">=" => greater_equal(state, &args),

				"mod" => mod_impl(state, &args),
				"and" => logical_and(state, &args),
				"or" => logical_or(state, &args),

				"list" => list(state, &args),

				"lookup" => lookup(state, &args),
				"len" => len_impl(state, &args),
				"slice" => slice_impl(state, &args),

				"print" => print_values(&args),

				"panic" => panic_impl(state, &args),

				name => {
					let symbol = state.read(name);
					match symbol {
						Value::Lambda(lambda) => lambda.call(state, &args),
						_ => runtime_error!(state, "Cannot call symbol {}", name),
					}
				}
			}
		}
	}
}

fn add_all(state: &mut ScopeState, values: &[Value]) -> Value {
	let mut result = 0;

	for value in values {
		match value {
			Value::I64(num) => result += num,
			_ => {
				runtime_error!(state, "Unsupported type of value passed to add");
			}
		}
	}

	Value::I64(result)
}

fn sub_all(state: &mut ScopeState, values: &[Value]) -> Value {
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

			_ => {
				runtime_error!(state, "Unsupported type of value passed to sub");
			}
		}
	}

	Value::I64(result.unwrap_or(0))
}

fn mul_all(state: &mut ScopeState, values: &[Value]) -> Value {
	let mut result = 1;

	for value in values {
		match value {
			Value::I64(num) => result *= num,
			_ => {
				runtime_error!(state, "Unsupported type of value passed to mul");
			}
		}
	}

	Value::I64(result)
}

fn div_all(state: &mut ScopeState, values: &[Value]) -> Value {
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

			_ => {
				runtime_error!(state, "Unsupported type of value passed to div");
			}
		}
	}

	Value::I64(result.unwrap_or(0))
}

fn all_equal(_: &mut ScopeState, values: &[Value]) -> Value {
	for index in 1..values.len() {
		if values[index - 1] != values[index] {
			return Value::Bool(false);
		}
	}

	Value::Bool(true)
}

fn all_not_equal(_: &mut ScopeState, values: &[Value]) -> Value {
	for index in 1..values.len() {
		if values[index - 1] == values[index] {
			return Value::Bool(false);
		}
	}

	Value::Bool(true)
}

fn invert_bool(_: &mut ScopeState, values: &[Value]) -> Value {
	match values.last() {
		Some(Value::Bool(value)) => Value::Bool(!value),
		_ => Value::Bool(false),
	}
}

fn is_truthy(value: &Value) -> bool {
	!matches!(value, Value::Bool(false))
}

fn logical_and(_: &mut ScopeState, values: &[Value]) -> Value {
	for index in 1..values.len() {
		let left = is_truthy(&values[index - 1]);
		let right = is_truthy(&values[index]);

		if !(left && right) {
			return Value::Bool(false);
		}
	}

	Value::Bool(true)
}

fn logical_or(_: &mut ScopeState, values: &[Value]) -> Value {
	for index in 1..values.len() {
		let left = is_truthy(&values[index - 1]);
		let right = is_truthy(&values[index]);

		if !(left || right) {
			return Value::Bool(false);
		}
	}

	Value::Bool(true)
}

macro_rules! two_arg_int_function {
	( $name:ident, $a:ident, $b:ident, $expr:expr, $return_type:ident ) => {
		fn $name(state: &mut ScopeState, values: &[Value]) -> Value {
			if values.len() != 2 {
				runtime_error!(
					state,
					"{} arguments to builtin function only taking two arguments",
					values.len()
				);
			}

			let $a = unwrap_integer(state, &values[0]);
			let $b = unwrap_integer(state, &values[1]);

			Value::$return_type($expr)
		}
	};
}

two_arg_int_function!(less, a, b, a < b, Bool);
two_arg_int_function!(less_equal, a, b, a <= b, Bool);
two_arg_int_function!(greater, a, b, a > b, Bool);
two_arg_int_function!(greater_equal, a, b, a >= b, Bool);

two_arg_int_function!(mod_impl, a, b, a % b, I64);

fn unwrap_integer(state: &mut ScopeState, value: &Value) -> i64 {
	match *value {
		Value::I64(value) => value,
		_ => {
			runtime_error!(state, "Expected integer");
			0
		}
	}
}

fn unwrap_usize(state: &mut ScopeState, value: i64) -> usize {
	if value < 0 {
		runtime_error!(state, "Negative integer used as index");
	}

	value as usize
}

fn list(_: &mut ScopeState, values: &[Value]) -> Value {
	Value::List(values.to_vec())
}

fn lookup(state: &mut ScopeState, values: &[Value]) -> Value {
	if values.len() != 2 {
		runtime_error!(
			state,
			"Expected 2 arguments to lookup, got {}",
			values.len()
		);
	}

	let value = &values[0];

	let index = unwrap_integer(state, &values[1]);
	let index = unwrap_usize(state, index);

	match value {
		Value::List(list) => {
			if index >= list.len() {
				runtime_error!(state, "Index {} out of bounds to lookup", index)
			} else {
				list[index].clone()
			}
		}
		_ => runtime_error!(state, "Cannot lookup on value which is not list"),
	}
}

fn slice_impl(state: &mut ScopeState, values: &[Value]) -> Value {
	if values.len() != 3 {
		runtime_error!(state, "Expected 3 arguments to slice, got {}", values.len());
	}

	let value = &values[0];

	let start = unwrap_integer(state, &values[1]);
	let start = unwrap_usize(state, start);

	let end = unwrap_integer(state, &values[2]);
	let end = unwrap_usize(state, end);

	if start >= end {
		return runtime_error!(state, "Start {} is past end {} when slicing", start, end);
	}

	match value {
		Value::List(list) => {
			if start >= list.len() {
				runtime_error!(state, "Start out of bounds to slice")
			} else if end > list.len() {
				runtime_error!(state, "End out of bounds to slice")
			} else {
				Value::List(list[start..end].to_vec())
			}
		}

		Value::String(string) => {
			if start >= string.len() {
				runtime_error!(state, "Start out of bounds to slice")
			} else if end > string.len() {
				runtime_error!(state, "End out of bounds to slice")
			} else {
				Value::String(string[start..end].to_string())
			}
		}

		_ => runtime_error!(state, "Cannot slice value which is not list or string"),
	}
}

fn len_impl(state: &mut ScopeState, values: &[Value]) -> Value {
	if values.len() != 1 {
		runtime_error!(state, "Expected 1 argument to len, got {}", values.len());
	}

	match &values[0] {
		Value::List(list) => Value::I64(list.len() as i64),
		Value::String(string) => Value::I64(string.len() as i64),
		_ => runtime_error!(state, "Cannot get len of value which is not list or string"),
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

fn insert_comment(state: &mut ScopeState, values: &[Value]) -> Value {
	state.insert_comment(values.to_vec());
	Value::None
}

fn repl(state: &mut ScopeState) {
	loop {
		print!("> ");
		std::io::stdout().flush().unwrap();

		let input = {
			let mut input = String::new();
			std::io::stdin().read_line(&mut input).unwrap();
			input
		};

		match input.as_str().trim() {
			"comments" => print_comments(state),

			"return" => break,
			"exit" => std::process::exit(0),

			input => {
				let tokens = tokenize(input);
				let tree = parse_expression(&mut tokens.iter().peekable());

				if let Ok(tree) = tree {
					let result = evaluate(state, &tree);
					print_values(&[result]);
				}
			}
		}
	}
}

fn panic_impl(state: &mut ScopeState, values: &[Value]) -> Value {
	if !values.is_empty() {
		print!("Panic!: ");

		let count = values.len();
		for (index, value) in values.iter().enumerate() {
			if index + 1 >= count {
				println!("{}", value);
			} else {
				print!("{} ", value);
			}
		}
	} else {
		println!("Panic!");
	}

	state.in_repl = true;
	repl(state);
	state.in_repl = false;

	std::process::exit(-1);
}

fn print_comments(state: &ScopeState) {
	let mut indent_level = 0;
	for scope_index in 0..state.scopes.len() {
		let scope = &state.scopes[scope_index];

		for comment in &scope.comments {
			for _ in 0..indent_level {
				print!("  ");
			}

			print_values(comment);
		}

		if !scope.comments.is_empty() {
			indent_level += 1;
		}
	}
}

fn main() {
	let source = std::fs::read_to_string("program.stack");
	let source = source.expect("Failed to find source file to execute");

	let tokens = tokenize(&source);
	let tree = parse_matching_parens(&mut tokens.iter().peekable());

	if let Ok(tree) = tree {
		let mut state = ScopeState::new();
		evaluate(&mut state, &tree);
	}
}
