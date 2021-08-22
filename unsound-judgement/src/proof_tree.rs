use crate::raw_ast as ast;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug)]
pub enum Item {
	Obligations {
		propositions: BTreeSet<ast::PropositionString>,
		proofs: Vec<ast::ProofExpr>,
	},
	Bindings {
		propositions: BTreeSet<ast::PropositionString>,
		bindings: Vec<ast::ProofBinding>,
	},
	RefAnnot {
		name: ast::ProofRefIdent,
		annot: ast::PropositionString,
	},
	RefProof {
		name: ast::ProofRefIdent,
		// don't need to store the actual proof; we only care that it was given
	},
	SubBranches {
		sub_branches: Vec<Branch>,
	},
}

#[derive(Clone, Debug)]
pub struct Branch {
	pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub struct Error(pub String);

#[derive(Clone, Debug)]
pub struct Summary {
	pub annotations: BTreeMap<ast::ProofRefIdent, ast::PropositionString>,
	pub proofs: BTreeSet<ast::ProofRefIdent>,
}

impl Summary {
	pub fn add_annotation(
		&mut self,
		name: &ast::ProofRefIdent,
		annot: &ast::PropositionString,
	) -> Result<(), Error> {
		if self.annotations.contains_key(name) {
			return Err(Error(format!(
				"proof ref @{} cannot be annotated with proposition {:?}, because it is \
                         also annotated with proposition {:?} elsewhere",
				&name.0, annot, &self.annotations[name]
			)));
		}
		self.annotations.insert(name.clone(), annot.clone());
		Ok(())
	}
}

impl Branch {
	pub fn process(&self) -> Result<Summary, Error> {
		let mut summary = Summary {
			annotations: BTreeMap::new(),
			proofs: BTreeSet::new(),
		};
		// First, collect everything
		for item in &self.items {
			match item {
				Item::SubBranches { sub_branches } => {
					let mut branches_proofs = BTreeSet::new();
					for sub_branch in sub_branches {
						let sub_summary = sub_branch.process()?;
						for (name, annot) in &sub_summary.annotations {
							summary.add_annotation(name, annot)?;
						}
						let intersect_proofs = branches_proofs
							.intersection(&sub_summary.proofs)
							.cloned()
							.collect::<BTreeSet<_>>();
						branches_proofs = intersect_proofs;
					}
					summary.proofs.extend(branches_proofs);
				}
				Item::RefProof { name } => {
					summary.proofs.insert(name.clone());
				}
				Item::RefAnnot { name, annot } => {
					summary.add_annotation(name, annot)?;
				}
				Item::Bindings {
					propositions: _,
					bindings,
				} => {
					for binding in bindings {
						if let Some(binding_prop) = &binding.proposition {
							summary.add_annotation(&binding.ident, binding_prop)?;
						}
					}
				}
				Item::Obligations {
					propositions: _,
					proofs,
				} => {
					for proof in proofs {
						if let ast::ProofExpr::Ref {
							ident,
							proposition: Some(proposition),
						} = proof
						{
							summary.add_annotation(ident, proposition)?;
						}
					}
				}
			}
		}
		// Next, check everything
		for item in &self.items {
			match item {
				Item::Bindings {
					propositions,
					bindings,
				} => {
					let mut bound_props = BTreeSet::new();
					for binding in bindings {
						if !summary.annotations.contains_key(&binding.ident) {
							return Err(Error(format!(
								"proof reference @{} not annotated with a proposition",
								&binding.ident.0
							)));
						}
						bound_props.insert(summary.annotations[&binding.ident].clone());
					}
					if !bound_props.is_subset(propositions) {
						return Err(Error("invalid postconditions".to_string()));
					}
				}
				Item::Obligations {
					propositions,
					proofs,
				} => {
					let mut proven_props = BTreeSet::new();
					for expr in proofs {
						match expr {
							ast::ProofExpr::Inline {
								proposition,
								proof: _,
							} => {
								proven_props.insert(proposition.clone());
							}
							ast::ProofExpr::Ref { ident, .. } => {
								if !summary.annotations.contains_key(ident) {
									return Err(Error(format!(
										"proof reference @{} not annotated with a proposition",
										ident.0
									)));
								}
								if !summary.proofs.contains(ident) {
									return Err(Error(format!("proof reference @{} not proven", ident.0)));
								}
								proven_props.insert(summary.annotations[ident].clone());
							}
						}
					}
					if !propositions.is_subset(&proven_props) {
						return Err(Error("invalid preconditions".to_string()));
					}
				}
				_ => {}
			}
		}
		Ok(summary)
	}
}
