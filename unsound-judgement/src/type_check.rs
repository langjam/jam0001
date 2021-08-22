use crate::raw_ast::{self as ast};
use std::{
	cell::RefCell,
	collections::{BTreeMap, BTreeSet},
	time::Instant,
};

#[derive(Clone, Debug)]
pub enum Error {
	Bad(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct NodeId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct PropositionsNodeId(usize);

#[derive(Clone, Debug)]
enum Node {
	Unknown,
	Unified(NodeId),
	RegularType {
		name: ast::TypeIdent,
		arguments: Vec<NodeId>,
	},
	TypeParameter(ast::TypeParameter),
	Function {
		parameters: Vec<NodeId>,
		result: NodeId,
		preconditions: PropositionsNodeId,
		postconditions: PropositionsNodeId,
	},
	Tuple(Vec<NodeId>),
}

#[derive(Clone, Debug)]
enum PropositionsNode {
	Unknown,
	Unified(PropositionsNodeId),
	Propositions(BTreeSet<ast::PropositionString>),
}

#[derive(Clone, Debug)]
struct Solver {
	// indexed by NodeId
	nodes: Vec<RefCell<Node>>,
	// indexed by PropositionNodeId
	proposition_nodes: Vec<RefCell<PropositionsNode>>,
}

impl Solver {
	fn new() -> Self {
		Solver {
			nodes: Vec::new(),
			proposition_nodes: Vec::new(),
		}
	}

	fn follow(&self, id: NodeId) -> Result<NodeId, Error> {
		let mut node = self.nodes[id.0]
			.try_borrow_mut()
			.map_err(|_| Error::Bad("encountered recursive type constraint".to_string()))?;
		if let Node::Unified(other) = &mut *node {
			let followed = self.follow(*other)?;
			*other = followed;
			Ok(followed)
		} else {
			Ok(id)
		}
	}

	fn unify(&self, actual: NodeId, expected: NodeId) -> Result<(), Error> {
		let actual_followed = self.follow(actual)?;
		let expected_followed = self.follow(expected)?;
		if actual_followed == expected_followed {
			return Ok(());
		}
		let mut actual_node = self.nodes[actual_followed.0]
			.try_borrow_mut()
			.map_err(|_| Error::Bad("encountered recursive type constraint".to_string()))?;
		let mut expected_node = self.nodes[expected_followed.0]
			.try_borrow_mut()
			.map_err(|_| Error::Bad("encountered recursive type constraint".to_string()))?;
		match (&mut *actual_node, &mut *expected_node) {
			(n @ Node::Unknown, _) => {
				*n = Node::Unified(expected_followed);
			}
			(_, n @ Node::Unknown) => {
				*n = Node::Unified(actual_followed);
			}
			(
				Node::RegularType {
					name: name1,
					arguments: arguments1,
				},
				Node::RegularType {
					name: name2,
					arguments: arguments2,
				},
			) => {
				if name1 != name2 {
					return Err(Error::Bad("type name mismatch".to_string()));
				}
				if arguments1.len() != arguments2.len() {
					return Err(Error::Bad(
						"type parameter list length mismatch".to_string(),
					));
				}
				for (&arg1, &arg2) in arguments1.iter().zip(arguments2.iter()) {
					self.unify(arg1, arg2)?;
				}
			}
			(Node::TypeParameter(param1), Node::TypeParameter(param2)) => {
				if param1 != param2 {
					return Err(Error::Bad("rigid type parameters do not match".to_string()));
				}
			}
			(
				Node::Function {
					parameters: parameters1,
					result: result1,
					preconditions: preconditions1,
					postconditions: postconditions1,
				},
				Node::Function {
					parameters: parameters2,
					result: result2,
					preconditions: preconditions2,
					postconditions: postconditions2,
				},
			) => {
				if parameters1.len() != parameters2.len() {
					return Err(Error::Bad(
						"function parameter list length mismatch".to_string(),
					));
				}
				for (&param1, &param2) in parameters1.iter().zip(parameters2.iter()) {
					self.unify(param1, param2)?;
				}
				self.unify(*result1, *result2)?;
				self.unify_propositions(*preconditions1, *preconditions2)?;
				self.unify_propositions(*postconditions1, *postconditions2)?;
			}
			(Node::Tuple(items1), Node::Tuple(items2)) => {
				if items1.len() != items2.len() {
					return Err(Error::Bad("tuple length mismatch".to_string()));
				}
				for (item1, item2) in items1.iter().zip(items2.iter()) {
					self.unify(*item1, *item2)?;
				}
			}
			(Node::Unified(_), _) | (_, Node::Unified(_)) => {
				unreachable!("follow should not return a Unified node")
			}
			_ => {
				return Err(Error::Bad("type mismatch".to_string()));
			}
		}
		Ok(())
	}

	fn follow_proposition(&self, id: PropositionsNodeId) -> PropositionsNodeId {
		let mut node = self.proposition_nodes[id.0].borrow_mut();
		if let PropositionsNode::Unified(other) = &mut *node {
			let followed = self.follow_proposition(*other);
			*other = followed;
			followed
		} else {
			id
		}
	}

	fn unify_propositions(
		&self,
		prop1: PropositionsNodeId,
		prop2: PropositionsNodeId,
	) -> Result<(), Error> {
		let prop1_followed = self.follow_proposition(prop1);
		let prop2_followed = self.follow_proposition(prop2);
		if prop1_followed == prop2_followed {
			return Ok(());
		}
		let mut prop1_node = self.proposition_nodes[prop1_followed.0].borrow_mut();
		let mut prop2_node = self.proposition_nodes[prop2_followed.0].borrow_mut();
		match (&mut *prop1_node, &mut *prop2_node) {
			(PropositionsNode::Unknown, _) => {
				*prop1_node = PropositionsNode::Unified(prop2_followed);
			}
			(_, PropositionsNode::Unknown) => {
				*prop2_node = PropositionsNode::Unified(prop1_followed);
			}
			(PropositionsNode::Propositions(props1), PropositionsNode::Propositions(prop2)) => {
				if props1.iter().collect::<BTreeSet<_>>() != prop2.iter().collect::<BTreeSet<_>>() {
					return Err(Error::Bad("Proposition mismatch".to_string()));
				}
			}
			(PropositionsNode::Unified(_), _) | (_, PropositionsNode::Unified(_)) => {
				unreachable!("follow should not return a Unified node")
			}
		}
		Ok(())
	}

	fn new_node(&mut self, node: Node) -> NodeId {
		let id = NodeId(self.nodes.len());
		self.nodes.push(RefCell::new(node));
		id
	}

	fn new_propositions_node(&mut self, node: PropositionsNode) -> PropositionsNodeId {
		let id = PropositionsNodeId(self.proposition_nodes.len());
		self.proposition_nodes.push(RefCell::new(node));
		id
	}
}

type TypeAliases = BTreeMap<ast::TypeIdent, (Vec<ast::TypeParameter>, ast::TypeExpr)>;

fn unfold_aliases(type_aliases: &TypeAliases, ty: &ast::TypeExpr) -> Result<ast::TypeExpr, Error> {
	fn unfold_rec(
		type_aliases: &TypeAliases,
		subst: Option<&BTreeMap<ast::TypeParameter, ast::TypeExpr>>,
		ty: &ast::TypeExpr,
	) -> Result<ast::TypeExpr, Error> {
		match ty {
			ast::TypeExpr::RegularType { name, arguments } => {
				let arguments_subst = arguments
					.iter()
					.map(|arg| unfold_rec(type_aliases, subst, arg))
					.collect::<Result<Vec<_>, _>>()?;
				match type_aliases.get(name) {
					Some((params, body)) => {
						if params.len() != arguments.len() {
							return Err(Error::Bad("wrong number of type arguments".to_string()));
						}
						// TODO: generate proper error for duplicate type parameter names
						let sub_subst = params
							.iter()
							.cloned()
							.zip(arguments_subst.into_iter())
							.collect::<BTreeMap<_, _>>();
						unfold_rec(type_aliases, Some(&sub_subst), body)
					}
					None => {
						// TODO: generate proper error when type is not defined
						Ok(ast::TypeExpr::RegularType {
							name: name.clone(),
							arguments: arguments_subst,
						})
					}
				}
			}
			ast::TypeExpr::TypeParameter(param) => match subst {
				Some(subst) => Ok(
					subst
						.get(param)
						.ok_or_else(|| Error::Bad("type parameter not in scope".to_string()))?
						.clone(),
				),
				None => Ok(ty.clone()),
			},
			ast::TypeExpr::Function {
				parameters,
				result,
				preconditions,
				postconditions,
			} => {
				let parameters_subst = parameters
					.iter()
					.map(|param| unfold_rec(type_aliases, subst, param))
					.collect::<Result<Vec<_>, _>>()?;
				let result_subst = unfold_rec(type_aliases, subst, result)?;
				Ok(ast::TypeExpr::Function {
					parameters: parameters_subst,
					result: Box::new(result_subst),
					preconditions: preconditions.clone(),
					postconditions: postconditions.clone(),
				})
			}
			ast::TypeExpr::Tuple(items) => {
				let items_subst = items
					.iter()
					.map(|item| unfold_rec(type_aliases, subst, item))
					.collect::<Result<Vec<_>, _>>()?;
				Ok(ast::TypeExpr::Tuple(items_subst))
			}
		}
	}

	unfold_rec(type_aliases, None, ty)
}

fn instantiate<E>(
	solver: &mut Solver,
	ty: &ast::TypeExpr,
	inst_func: &mut impl for<'a> FnMut(&'a mut Solver, &'a ast::TypeParameter) -> Result<NodeId, E>,
) -> Result<NodeId, E> {
	Ok(match ty {
		// We assume type aliases have already been expanded
		ast::TypeExpr::RegularType { name, arguments } => {
			let arguments_inst = arguments
				.iter()
				.map(|arg| instantiate(solver, arg, inst_func))
				.collect::<Result<_, _>>()?;
			solver.new_node(Node::RegularType {
				name: name.clone(),
				arguments: arguments_inst,
			})
		}
		ast::TypeExpr::TypeParameter(param) => inst_func(solver, param)?,
		ast::TypeExpr::Function {
			parameters,
			result,
			preconditions,
			postconditions,
		} => {
			let parameters_inst = parameters
				.iter()
				.map(|param| instantiate(solver, param, inst_func))
				.collect::<Result<_, _>>()?;
			let result_inst = instantiate(solver, result, inst_func)?;
			let preconditions_inst = solver.new_propositions_node(PropositionsNode::Propositions(
				preconditions.iter().cloned().collect(),
			));
			let postconditions_inst = solver.new_propositions_node(PropositionsNode::Propositions(
				postconditions.iter().cloned().collect(),
			));
			solver.new_node(Node::Function {
				parameters: parameters_inst,
				result: result_inst,
				preconditions: preconditions_inst,
				postconditions: postconditions_inst,
			})
		}
		ast::TypeExpr::Tuple(items) => {
			let items_inst = items
				.iter()
				.map(|item| instantiate(solver, item, inst_func))
				.collect::<Result<_, _>>()?;
			solver.new_node(Node::Tuple(items_inst))
		}
	})
}

fn instantiate_polymorphic(solver: &mut Solver, poly_type: &ast::TypeExpr) -> NodeId {
	let mut type_params = BTreeMap::new();
	instantiate(solver, poly_type, &mut |solver,
	                                     param|
	 -> Result<
		_,
		std::convert::Infallible,
	> {
		Ok(
			*type_params
				.entry(param.clone())
				.or_insert_with(|| solver.new_node(Node::Unknown)),
		)
	})
	.unwrap()
}

fn instantiate_rigid(solver: &mut Solver, ty: &ast::TypeExpr) -> NodeId {
	instantiate(solver, ty, &mut |solver,
	                              param|
	 -> Result<_, std::convert::Infallible> {
		Ok(solver.new_node(Node::TypeParameter(param.clone())))
	})
	.unwrap()
}

type Constructors =
	BTreeMap<ast::Var, (ast::TypeIdent, Vec<ast::TypeParameter>, Vec<ast::TypeExpr>)>;

fn check_block(
	type_aliases: &TypeAliases,
	constructors: &Constructors,
	globals: &mut BTreeMap<ast::Var, ast::TypeExpr>,
	locals: &mut BTreeMap<ast::Var, NodeId>,
	solver: &mut Solver,
	block: &ast::Block,
	expected: NodeId,
) -> Result<(), Error> {
	let mut to_remove = Vec::new();
	for stat in &block.statements {
		match stat {
			ast::Statement::Expr(expr) => {
				let fresh_var = solver.new_node(Node::Unknown);
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					expr,
					fresh_var,
				)?;
			}
			ast::Statement::Let { lhs, rhs } => {
				let fresh_var = solver.new_node(Node::Unknown);
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					rhs,
					fresh_var,
				)?;
				if locals.contains_key(lhs) {
					return Err(Error::Bad("local already defined".to_string()));
				}
				locals.insert(lhs.clone(), fresh_var);
				to_remove.push(lhs);
			}
			ast::Statement::LetTuple { lhs, rhs } => {
				let fresh_vars = (0..lhs.len())
					.map(|_| solver.new_node(Node::Unknown))
					.collect::<Vec<_>>();
				let fresh_tuple = solver.new_node(Node::Tuple(fresh_vars.clone()));
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					rhs,
					fresh_tuple,
				)?;
				for (local, ty) in lhs.iter().zip(fresh_vars.iter()) {
					if let Some(local) = local {
						if locals.contains_key(local) {
							return Err(Error::Bad("local already defined".to_string()));
						}
						locals.insert(local.clone(), *ty);
						to_remove.push(local);
					}
				}
			}
			ast::Statement::Proof { .. } => {
				// no op
			}
			ast::Statement::Known { .. } => {
				// no op
			}
		}
	}
	check_expr(
		type_aliases,
		constructors,
		globals,
		locals,
		solver,
		&block.result,
		expected,
	)?;
	for local in to_remove {
		locals.remove(local);
	}
	Ok(())
}

fn check_expr(
	type_aliases: &TypeAliases,
	constructors: &Constructors,
	globals: &mut BTreeMap<ast::Var, ast::TypeExpr>,
	locals: &mut BTreeMap<ast::Var, NodeId>,
	solver: &mut Solver,
	expr: &ast::Expr,
	expected: NodeId,
) -> Result<(), Error> {
	match expr {
		ast::Expr::Var(var) => {
			if let Some(local_ty) = locals.get(var) {
				solver.unify(*local_ty, expected)?;
			} else if let Some(global) = globals.get(var) {
				let global_ty = instantiate_polymorphic(solver, &unfold_aliases(type_aliases, global)?);
				solver.unify(global_ty, expected)?;
			} else {
				return Err(Error::Bad("variable not found".to_string()));
			}
		}
		ast::Expr::Call {
			callee,
			args,
			preconditions: _,
			postconditions: _,
		} => {
			let arg_tys = (0..args.len())
				.map(|_| solver.new_node(Node::Unknown))
				.collect::<Vec<_>>();
			let ret_ty = solver.new_node(Node::Unknown);
			let pre_conds = solver.new_propositions_node(PropositionsNode::Unknown);
			let post_conds = solver.new_propositions_node(PropositionsNode::Unknown);
			let func_expected = solver.new_node(Node::Function {
				parameters: arg_tys.clone(),
				result: ret_ty,
				preconditions: pre_conds,
				postconditions: post_conds,
			});
			check_expr(
				type_aliases,
				constructors,
				globals,
				locals,
				solver,
				callee,
				func_expected,
			)?;
			for (arg, arg_expected) in args.iter().zip(arg_tys.iter()) {
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					arg,
					*arg_expected,
				)?;
			}
			solver.unify(ret_ty, expected)?;
		}
		ast::Expr::Array(items) => {
			let item_ty = solver.new_node(Node::Unknown);
			let array_ty = solver.new_node(Node::RegularType {
				name: ast::TypeIdent::Array,
				arguments: vec![item_ty],
			});
			for item in items {
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					item,
					item_ty,
				)?;
			}
			solver.unify(array_ty, expected)?;
		}
		ast::Expr::Tuple(items) => {
			let items_expected = (0..items.len())
				.map(|_| solver.new_node(Node::Unknown))
				.collect::<Vec<_>>();
			let tuple_ty = solver.new_node(Node::Tuple(items_expected.clone()));
			solver.unify(tuple_ty, expected)?;
			for (item, item_expected) in items.iter().zip(items_expected) {
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					item,
					item_expected,
				)?;
			}
		}
		ast::Expr::Lambda {
			parameters,
			ret_ty,
			preconditions,
			postconditions,
			body,
		} => {
			let parameter_nodes = parameters
				.iter()
				.map(|_| solver.new_node(Node::Unknown))
				.collect::<Vec<_>>();
			let ret_node = if let Some(ret_ty) = ret_ty {
				instantiate_rigid(solver, &unfold_aliases(type_aliases, ret_ty)?)
			} else {
				solver.new_node(Node::Unknown)
			};
			let pre_conds_node = if preconditions.is_empty() {
				solver.new_propositions_node(PropositionsNode::Unknown)
			} else {
				let props = preconditions
					.iter()
					.map(|pre_cond| pre_cond.proposition.clone())
					.collect::<BTreeSet<_>>();
				solver.new_propositions_node(PropositionsNode::Propositions(props))
			};
			let post_conds_node = if postconditions.is_empty() {
				solver.new_propositions_node(PropositionsNode::Unknown)
			} else {
				let props = postconditions
					.iter()
					.map(|post_cond| post_cond.proposition.clone())
					.collect::<BTreeSet<_>>();
				solver.new_propositions_node(PropositionsNode::Propositions(props))
			};
			let lam_expected = solver.new_node(Node::Function {
				parameters: parameter_nodes.clone(),
				result: ret_node,
				preconditions: pre_conds_node,
				postconditions: post_conds_node,
			});
			solver.unify(lam_expected, expected)?;
			for (param, param_node) in parameters.iter().zip(parameter_nodes.iter()) {
				if locals.contains_key(param) {
					return Err(Error::Bad("local already defined".to_string()));
				}
				locals.insert(param.clone(), *param_node);
			}
			check_block(
				type_aliases,
				constructors,
				globals,
				locals,
				solver,
				body,
				ret_node,
			)?;
			for param in parameters {
				locals.remove(param);
			}
		}
		ast::Expr::Match { discrim, cases } => {
			let discrim_ty = solver.new_node(Node::Unknown);
			check_expr(
				type_aliases,
				constructors,
				globals,
				locals,
				solver,
				discrim,
				discrim_ty,
			)?;
			for (pat, body) in cases {
				let mut to_remove = Vec::new();
				if let ast::Pattern::Constructor { name, elements } = pat {
					let (ctor_ty, ctor_params, ctor_fields) = constructors
						.get(name)
						.ok_or_else(|| Error::Bad("constructor not found".to_string()))?;
					let ctor_param_nodes = (0..ctor_params.len())
						.map(|_| solver.new_node(Node::Unknown))
						.collect::<Vec<_>>();
					let ctor_ty_node = solver.new_node(Node::RegularType {
						name: ctor_ty.clone(),
						arguments: ctor_param_nodes.clone(),
					});
					solver.unify(ctor_ty_node, discrim_ty)?;
					let subst = ctor_params
						.iter()
						.cloned()
						.zip(ctor_param_nodes.iter().cloned())
						.collect::<BTreeMap<_, _>>();
					let ctor_fields_subst = ctor_fields
						.iter()
						.map(|field_ty| {
							let unfolded = unfold_aliases(type_aliases, field_ty)?;
							instantiate(solver, &unfolded, &mut |_solver, param| {
								Ok(*subst.get(param).ok_or_else(|| {
									Error::Bad("type parameter in constructor field type not found".to_string())
								})?)
							})
						})
						.collect::<Result<Vec<_>, _>>()?;
					if ctor_fields.len() != elements.len() {
						return Err(Error::Bad("constructor field count mismatch".to_string()));
					}
					for (field, field_ty) in elements.iter().zip(ctor_fields_subst.iter()) {
						if let Some(field) = field {
							if locals.contains_key(field) {
								return Err(Error::Bad("local already defined".to_string()));
							}
							locals.insert(field.clone(), *field_ty);
							to_remove.push(field);
						}
					}
				}
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					body,
					expected,
				)?;
				for local in to_remove {
					locals.remove(local);
				}
			}
		}
		ast::Expr::Magic { name: _, params } => {
			for param in params {
				let fresh_var = solver.new_node(Node::Unknown);
				check_expr(
					type_aliases,
					constructors,
					globals,
					locals,
					solver,
					param,
					fresh_var,
				)?;
			}
		}
		ast::Expr::StringLit(_) => {
			let string_node = solver.new_node(Node::RegularType {
				name: ast::TypeIdent::Var("string".to_string()),
				arguments: vec![],
			});
			solver.unify(string_node, expected)?;
		}
		ast::Expr::IntLit(_) => {
			let integer_node = solver.new_node(Node::RegularType {
				name: ast::TypeIdent::Var("integer".to_string()),
				arguments: vec![],
			});
			solver.unify(integer_node, expected)?;
		}
		ast::Expr::Block(block) => {
			check_block(
				type_aliases,
				constructors,
				globals,
				locals,
				solver,
				block,
				expected,
			)?;
		}
	}
	Ok(())
}

pub fn check_program(program: &ast::Program) -> Result<(), Error> {
	// First pass; gather declarations
	let mut type_aliases = BTreeMap::new();
	let mut sum_types = BTreeSet::<ast::TypeIdent>::new();
	let mut constructors = BTreeMap::new();
	let mut globals = BTreeMap::new();
	for def in &program.definitions {
		match def {
			ast::Definition::TypeAlias {
				name,
				parameters,
				content,
			} => {
				if type_aliases.contains_key(name) {
					return Err(Error::Bad("type alias already defined".to_string()));
				}
				type_aliases.insert(name.clone(), (parameters.clone(), content.clone()));
			}
			ast::Definition::SumType {
				name,
				parameters,
				constructors: this_ctors,
			} => {
				if sum_types.contains(name) {
					return Err(Error::Bad("sum type already defined".to_string()));
				}
				sum_types.insert(name.clone());
				for (ctor_name, ctor_fields) in this_ctors {
					if constructors.contains_key(ctor_name) {
						return Err(Error::Bad("constructor already defined".to_string()));
					}
					constructors.insert(
						ctor_name.clone(),
						(name.clone(), parameters.clone(), ctor_fields.clone()),
					);
				}
			}
			ast::Definition::Let { name, ty, body: _ } => {
				globals.insert(name.clone(), ty.clone());
			}
		}
	}
	for def in &program.definitions {
		if let ast::Definition::Let { name: _, ty, body } = def {
			let mut solver = Solver::new();
			let ty_node = instantiate_rigid(&mut solver, ty);
			let mut locals = BTreeMap::new();
			check_expr(
				&type_aliases,
				&constructors,
				&mut globals,
				&mut locals,
				&mut solver,
				body,
				ty_node,
			)?;
		}
	}
	Ok(())
}
