use crate::raw_ast as ast;
use std::cell::RefCell;

#[derive(Clone, Debug)]
enum Error {
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
	Propositions(Vec<ast::PropositionString>),
}

#[derive(Clone, Debug)]
struct Context {
	// indexed by NodeId
	nodes: Vec<RefCell<Node>>,
	// indexed by PropositionNodeId
	proposition_nodes: Vec<RefCell<Node>>,
}

impl Context {
	fn new() -> Self {
		Context {
			nodes: Vec::new(),
			proposition_nodes: Vec::new(),
		}
	}

	fn follow(&self, id: NodeId) -> Result<NodeId, Error> {
		let mut node = self.nodes[id.0]
			.try_borrow_mut()
			.map_err(|_| Error::Bad("weird recursive type constraint".to_string()))?;
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
			.map_err(|_| Error::Bad("weird recursive type constraint".to_string()))?;
		let mut expected_node = self.nodes[expected_followed.0]
			.try_borrow_mut()
			.map_err(|_| Error::Bad("weird recursive type constraint".to_string()))?;
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
					return Err(Error::Bad(
						"type name mismatch -- fix it!!! >:[".to_string(),
					));
				}
				if arguments1.len() != arguments2.len() {
					return Err(Error::Bad("lens don't match.......".to_string()));
				}
				for (&arg1, &arg2) in arguments1.iter().zip(arguments2.iter()) {
					self.unify(arg1, arg2)?;
				}
			}
			_ => {
				return Err(Error::Bad("type mismatch -- fix it!! >:(".to_string()));
			}
		}
		Ok(())
	}
}
