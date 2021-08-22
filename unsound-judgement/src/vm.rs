#![allow(irrefutable_let_patterns)]

use crate::raw_ast;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
	parameters: usize,
	closure: Vec<Value>,
	body: Block,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantConstructor {
	parameters: usize,
	tag: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
	Integer(i64),
	String(String),
	Array(Vec<Value>),
	Tuple(Vec<Value>),
	Variant { tag: i64, elements: Vec<Value> },
	Function(Function),
	NullaryVariant { tag: i64 },
	VariantConstructor(VariantConstructor),
}

#[derive(Clone)]
struct GlobalState {
	globals: BTreeMap<String, Option<Value>>,
	magic_functions: BTreeMap<&'static str, fn(&[Value]) -> Value>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
	Local(usize),
	Global(String),
	Call {
		callee: Box<Expr>,
		args: Vec<Expr>,
	},
	// array(x, y, z)
	Array(Vec<Expr>),
	// (x, y, ...)
	Tuple(Vec<Expr>),
	// fn (x, y) { ... }
	Lambda {
		parameters: usize,
		body: Block,
	},
	Match {
		discrim: Box<Expr>,
		cases: Vec<(VariantConstructor, Expr)>,
		any_case: Option<Box<Expr>>,
	},
	// __magic("asdf", ...)
	Magic {
		name: String,
		params: Vec<Expr>,
	},
	StringLit(String),
	IntLit(i64),
	Block(Block),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Statement {
	Expr(Expr),
	Let(Box<Expr>),
	LetTuple { count: usize, expr: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Block {
	statements: Vec<Statement>,
	result: Box<Expr>,
}

macro_rules! magic_function_internal {
	($args:ident [$i:expr]; () => $e:expr) => (
		$e
	);
	($args:ident [$i:expr]; ($p1:pat) => $e:expr) => (
		if $i >= $args.len() {
			panic!("Invalid number of parameters to magic function")
		} else if let $p1 = $args[$i] {
			$e
		} else {
			panic!("Invalid argument to magic function")
		}
	);
	($args:ident [$i:expr]; ($p1:pat, $($ps:pat),*) => $e:expr) => (
		if $i >= $args.len() {
			panic!("Invalid number of parameters to magic function")
		} else if let $p1 = $args[$i] {
			magic_function_internal!($args [$i + 1]; ($($ps),*) => $e)
		} else {
			panic!("Invalid argument to magic function")
		}
	);
}
macro_rules! magic_function {
	(($($p:pat),*) => $e:expr) => ({
		let f: fn(&[Value]) -> Value = |args| {
			magic_function_internal!(args[0]; ($($p),*) => $e)
		};
		f
	});
}

fn magic_print(v: &Value) {
	let print_arr = |arr| {
		let mut first = true;
		for v in arr {
			if first {
				first = false;
			} else {
				print!(", ");
			}
			magic_print(v);
		}
	};

	match v {
		Value::Integer(i) => print!("{}", i),
		Value::String(s) => print!("{}", s),
		Value::Array(arr) => {
			print!("[");
			print_arr(arr);
			print!("]");
		}
		Value::Tuple(tup) => {
			print!("(");
			print_arr(tup);
			print!(")");
		}
		Value::NullaryVariant { tag } => {
			print!("variant {}", tag)
		}
		Value::Variant { tag, elements } => {
			print!("variant {}(", tag);
			print_arr(elements);
			print!(")")
		}
		Value::Function(_) => print!("<function>"),
		Value::VariantConstructor(_) => print!("<ctor>"),
	}
}

fn get_magic_functions() -> BTreeMap<&'static str, fn(&[Value]) -> Value> {
	let mut res = BTreeMap::new();
	res.insert(
		"add",
		magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y)),
	);
	res.insert(
		"sub",
		magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y)),
	);
	res.insert(
		"mul",
		magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Integer(x * y)),
	);
	res.insert(
		"div",
		magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Integer(x / y)),
	);
	res.insert(
		"eq",
		magic_function!((ref x, ref y) => Value::Variant { tag: (x == y) as i64, elements: vec![] }),
	);
	res.insert(
		"ne",
		magic_function!((ref x, ref y) => Value::Variant { tag: (x != y) as i64, elements: vec![] }),
	);
	res.insert(
"lt",
magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Variant { tag: (x < y) as i64, elements: vec![] }),
);
	res.insert(
"gt",
magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Variant { tag: (x > y) as i64, elements: vec![] }),
);
	res.insert(
"le",
magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Variant { tag: (x <= y) as i64, elements: vec![] }),
);
	res.insert(
"ge",
magic_function!((Value::Integer(x), Value::Integer(y)) => Value::Variant { tag: (x >= y) as i64, elements: vec![] }),
);
	res.insert(
		"get",
		magic_function!((Value::Array(ref arr), Value::Integer(idx)) => arr[idx as usize].clone()),
	);
	res.insert(
		"len",
		magic_function!((Value::Array(ref arr)) => Value::Integer(arr.len() as i64)),
	);
	res.insert(
		"print",
		magic_function!((ref v) => {
			magic_print(v);
			Value::Tuple(vec![])
		}),
	);
	res
}
fn vmify_program(p: raw_ast::Program) -> GlobalState {
	use raw_ast::Definition;

	let mut gs = GlobalState {
		globals: BTreeMap::new(),
		magic_functions: get_magic_functions(),
	};
	let mut bodies_to_insert: Vec<(String, raw_ast::Expr)> = vec![];

	for definition in p.definitions {
		match definition {
			Definition::SumType { constructors, .. } => {
				for (index, (name, params)) in constructors.iter().enumerate() {
					if params.is_empty() {
						gs.globals.insert(
							name.0.clone(),
							Some(Value::NullaryVariant { tag: index as i64 }),
						);
					} else {
						gs.globals.insert(
							name.0.clone(),
							Some(Value::VariantConstructor(VariantConstructor {
								parameters: params.len(),
								tag: index as i64,
							})),
						);
					}
				}
			}
			Definition::Let { name, body, .. } => {
				gs.globals.insert(name.0.clone(), None);
				bodies_to_insert.push((name.0, body));
			}
			Definition::TypeAlias { .. } => {}
		}
	}

	for (name, expr) in &bodies_to_insert {
		let expr = vmify_expr(&gs, expr, &[]);
		let value = evaluate_expression(&gs, &expr, &[]);
		*gs.globals.get_mut(name).unwrap() = Some(value);
	}
	gs
}

fn resolve_name(gs: &GlobalState, name: &str, names: &[String]) -> Expr {
	let mut index = names.len();
	for name_ in names.iter().rev() {
		index -= 1;
		if name == name_ {
			return Expr::Local(index);
		}
	}

	if gs.globals.get(name).is_some() {
		Expr::Global(name.to_owned())
	} else {
		panic!("Failed to resolve name {}", name)
	}
}
fn vmify_expr(gs: &GlobalState, expr: &raw_ast::Expr, names: &[String]) -> Expr {
	use raw_ast::Expr as RawExpr;
	let vmify_args = |args: &[raw_ast::Expr]| -> Vec<Expr> {
		args.iter().map(|arg| vmify_expr(gs, arg, names)).collect()
	};

	match expr {
		RawExpr::Var(v) => resolve_name(gs, &v.0, names),
		RawExpr::Call { callee, args, .. } => Expr::Call {
			callee: Box::new(vmify_expr(gs, callee, names)),
			args: vmify_args(args),
		},
		RawExpr::Array(args) => Expr::Array(vmify_args(args)),
		RawExpr::Tuple(args) => Expr::Tuple(vmify_args(args)),
		RawExpr::Lambda {
			parameters, body, ..
		} => {
			let names: Vec<String> = names
				.iter()
				.chain(parameters.iter().map(|v| &v.0))
				.cloned()
				.collect();
			Expr::Lambda {
				parameters: parameters.len(),
				body: vmify_block(gs, body, &names),
			}
		}
		RawExpr::Match {
			discrim,
			cases: raw_cases,
		} => {
			let discrim = Box::new(vmify_expr(gs, discrim, names));
			let mut cases = vec![];
			let mut any_case = None;
			for (pattern, expr) in raw_cases {
				match pattern {
					raw_ast::Pattern::Any => {
						if any_case.is_some() {
							panic!("Multiple any cases")
						} else {
							any_case = Some(Box::new(vmify_expr(gs, expr, names)))
						}
					}
					raw_ast::Pattern::Constructor { name, elements } => {
						match gs.globals.get(&name.0) {
							Some(Some(Value::VariantConstructor(ctor))) => {
								assert!(elements.len() == ctor.parameters);
								let mut new_names = names.to_vec();
								for name in elements {
									if let Some(name) = name {
										new_names.push(name.0.clone())
									} else {
										new_names.push(String::new())
									}
								}
								cases.push((ctor.clone(), vmify_expr(gs, expr, &new_names)));
							}
							Some(Some(Value::NullaryVariant { tag })) => cases.push((
								VariantConstructor {
									tag: *tag,
									parameters: 0,
								},
								vmify_expr(gs, expr, names),
							)),
							Some(Some(otherwise)) => panic!(
								"match case must be a variant constructor (is {:?})",
								otherwise
							),
							Some(None) => panic!("match case must be a variant constructor (is uninitialized)"),
							None => panic!("Unknown variant constructor name \"{}\"", name.0),
						};
					}
				}
			}

			Expr::Match {
				discrim,
				cases,
				any_case,
			}
		}
		RawExpr::Magic { name, params } => Expr::Magic {
			name: name.clone(),
			params: vmify_args(params),
		},
		RawExpr::StringLit(s) => Expr::StringLit(s.clone()),
		RawExpr::IntLit(i) => Expr::IntLit(*i),
		RawExpr::Block(b) => Expr::Block(vmify_block(gs, b, names)),
	}
}
fn vmify_block(gs: &GlobalState, block: &raw_ast::Block, names: &[String]) -> Block {
	use raw_ast::Statement as RawStatement;

	let mut names = names.to_vec();
	let mut statements = vec![];
	for stmt in &block.statements {
		match stmt {
			RawStatement::Expr(e) => statements.push(Statement::Expr(vmify_expr(gs, e, &names))),
			RawStatement::Let { lhs, rhs } => {
				statements.push(Statement::Let(Box::new(vmify_expr(gs, rhs, &names))));
				names.push(lhs.0.clone());
			}
			// let (x, y) = ...;
			RawStatement::LetTuple { lhs, rhs } => {
				statements.push(Statement::LetTuple {
					count: lhs.len(),
					expr: Box::new(vmify_expr(gs, rhs, &names)),
				});
				for name in lhs {
					if let Some(v) = name {
						names.push(v.0.clone())
					} else {
						names.push(String::new())
					}
				}
			}
			_ => (),
		}
	}
	Block {
		statements,
		result: Box::new(vmify_expr(gs, &block.result, &names)),
	}
}

pub fn evaluate(p: raw_ast::Program) -> Value {
	let gs = vmify_program(p);

	let main = &gs.globals.get("main");
	match main {
		Some(Some(Value::Function(f))) => evaluate_function(&gs, f, &[]),
		Some(Some(v)) => panic!("main must be a function! (is {:?})", v),
		Some(None) => panic!("main must be a function! (is undefined)"),
		None => panic!("main is not defined"),
	}
}

fn evaluate_function(gs: &GlobalState, f: &Function, params: &[Value]) -> Value {
	if f.parameters != params.len() {
		panic!("Incorrect number of parameters passed to function")
	}

	let stack = params
		.iter()
		.cloned()
		.chain(f.closure.iter().cloned())
		.collect();
	evaluate_block(gs, &f.body, stack)
}

fn evaluate_block(gs: &GlobalState, b: &Block, mut stack: Vec<Value>) -> Value {
	for stmt in &b.statements {
		match stmt {
			Statement::Expr(e) => std::mem::drop(evaluate_expression(gs, e, &stack)),
			Statement::Let(e) => {
				let value = evaluate_expression(gs, e, &stack);
				stack.push(value);
			}
			Statement::LetTuple { count, expr } => {
				let value = evaluate_expression(gs, expr, &stack);
				if let Value::Tuple(v) = value {
					if *count != v.len() {
						panic!(
							"Attempted to let-tuple a tuple of the wrong size ({} != {})",
							count,
							v.len()
						)
					} else {
						stack.extend(v)
					}
				} else {
					panic!("Attempted to let-tuple a non-tuple ({:?})", value)
				}
			}
		}
	}

	evaluate_expression(gs, &b.result, &stack)
}

fn evaluate_expression(gs: &GlobalState, e: &Expr, stack: &[Value]) -> Value {
	let evaluate_args = |args: &[Expr]| -> Vec<Value> {
		args
			.iter()
			.map(|e| evaluate_expression(gs, e, stack))
			.collect()
	};
	match e {
		Expr::Local(n) => stack[*n].clone(),
		Expr::Global(name) => gs.globals[name].clone().unwrap(),
		Expr::Call { callee, args } => {
			let args = evaluate_args(args);
			match evaluate_expression(gs, callee, stack) {
				Value::Function(ref f) => evaluate_function(gs, f, &args),
				Value::VariantConstructor(VariantConstructor { parameters, tag }) => {
					if parameters != args.len() {
						panic!("Incorrect number of args passed to variant constructor")
					} else {
						Value::Variant {
							tag,
							elements: args,
						}
					}
				}
				v => panic!("Attempted to call a non-function (called {:?})", v),
			}
		}
		// array(x, y, z)
		Expr::Array(args) => Value::Array(evaluate_args(args)),
		// (x, y, ...)
		Expr::Tuple(args) => Value::Tuple(evaluate_args(args)),
		// fn (x, y) { ... }
		Expr::Lambda { parameters, body } => Value::Function(Function {
			parameters: *parameters,
			closure: stack.to_vec(),
			body: body.clone(),
		}),
		Expr::Match {
			discrim,
			cases,
			any_case,
		} => {
			let discrim = evaluate_expression(gs, discrim, stack);
			if let Value::Variant { tag, mut elements } = discrim {
				let mut value = None;
				for (case, expr) in cases {
					if tag == case.tag {
						if case.parameters != elements.len() {
							panic!(
								"Match case has incorrect number of parameters ({} != {})",
								case.parameters,
								elements.len()
							)
						} else {
							let mut new_stack = stack.to_vec();
							new_stack.extend(std::mem::replace(&mut elements, vec![]));
							value = Some(evaluate_expression(gs, expr, &new_stack));
							break;
						}
					}
				}

				if let Some(value) = value {
					value
				} else if let Some(any_case) = any_case {
					evaluate_expression(gs, any_case, &stack)
				} else {
					panic!(
						"Non-exhaustive match - failed to match discriminator tag {}",
						tag
					)
				}
			} else {
				panic!("Attempted to match on a non-variant ({:?})", discrim)
			}
		}
		// __magic("asdf", ...)
		Expr::Magic { name, params } => {
			let magic_fn = gs.magic_functions.get(&name[..]);
			if let Some(magic_fn) = magic_fn {
				magic_fn(&evaluate_args(params))
			} else {
				panic!("Unknown magic function: \"{}\"", name)
			}
		}
		Expr::StringLit(s) => Value::String(s.clone()),
		Expr::IntLit(i) => Value::Integer(*i),
		Expr::Block(b) => evaluate_block(gs, b, stack.to_vec()),
	}
}
