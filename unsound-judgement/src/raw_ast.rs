#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProofRefIdent(pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PropositionString(pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProofString(pub String);

#[derive(Clone, Debug)]
pub enum ProofExpr {
	// f(x) [@foo: `bar`] or f(x) [@foo]
	Ref {
		ident: ProofRefIdent,
		proposition: Option<PropositionString>,
	},
	// f(x) [proof `...` = `...`]
	Inline {
		proposition: PropositionString,
		proof: ProofString,
	},
}

// [-> @foo] or [-> @foo: `bar`]
#[derive(Clone, Debug)]
pub struct ProofBinding {
	pub ident: ProofRefIdent,
	pub proposition: Option<PropositionString>,
}

#[derive(Clone, Debug)]
pub struct ProofParam {
	pub ident: Option<ProofRefIdent>,
	pub proposition: PropositionString,
}

#[derive(Clone, Debug)]
pub struct ProofResult {
	pub ident: ProofRefIdent,
	pub proposition: PropositionString,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeIdent {
	Var(String),
	Array,
	Tuple,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeParameter(pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarDecl {
	pub name: Var,
	pub ty: Option<TypeExpr>,
}

#[derive(Clone, Debug)]
pub enum Pattern {
	Any,
	Constructor {
		name: Var,
		elements: Vec<Option<Var>>
	},
}

#[derive(Clone, Debug)]
pub enum Expr {
	// foo
	Var(Var),
	// foo(x, y, z) [@blah -> @foo]
	Call {
		callee: Box<Expr>,
		args: Vec<Expr>,
		preconditions: Vec<ProofExpr>,
		postconditions: Vec<ProofBinding>,
	},
	// array(x, y, z)
	Array(Vec<Expr>),
	// (x, y, ...)
	Tuple(Vec<Expr>),
	// fn (x, y) { ... }
	Lambda {
		parameters: Vec<Var>,
		ret_ty: Option<TypeExpr>,
		preconditions: Vec<ProofParam>,
		postconditions: Vec<ProofResult>,
		body: Block,
	},
	Match {
		discrim: Box<Expr>,
		cases: Vec<(Pattern, Expr)>,
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

#[derive(Clone, Debug)]
pub enum Statement {
	// ...;
	Expr(Expr),
	// let x = ...;
	Let {
		lhs: Var,
		rhs: Expr,
	},
	// let (x, y) = ...;
	LetTuple {
		lhs: Vec<Option<Var>>,
		rhs: Expr,
	},
	// proof @foo : `bar` = baz;
	Proof {
		ident: ProofRefIdent,
		proposition: Option<PropositionString>,
		proof: ProofString,
	},
	// known @blah: `...`
	Known {
		ident: ProofRefIdent,
		proposition: PropositionString,
	},
}

#[derive(Clone, Debug)]
pub struct Block {
	pub statements: Vec<Statement>,
	pub result: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeExpr {
	RegularType {
		name: TypeIdent,
		arguments: Vec<TypeExpr>,
	},
	TypeParameter(TypeParameter),
	Function {
		parameters: Vec<TypeExpr>,
		result: Box<TypeExpr>,
		preconditions: Vec<PropositionString>,
		postconditions: Vec<PropositionString>,
	},
	Tuple(Vec<TypeExpr>),
}

#[derive(Clone, Debug)]
pub enum Definition {
	// type name('a, 'b) = ...;
	TypeAlias {
		name: TypeIdent,
		parameters: Vec<TypeParameter>,
		content: TypeExpr,
	},
	/*
		type name('a, 'b) = variant {
			...
		};
	*/
	SumType {
		name: TypeIdent,
		parameters: Vec<TypeParameter>,
		constructors: Vec<(Var, Vec<TypeExpr>)>,
	},
	// let name: type = ...;
	// fn name(...) -> ... {}
	Let {
		name: Var,
		ty: TypeExpr,
		body: Expr,
	},
}

#[derive(Clone, Debug)]
pub struct Program {
	pub definitions: Vec<Definition>,
}
