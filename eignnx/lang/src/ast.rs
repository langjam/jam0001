use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Loc;

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "file:? ?:?")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tm {
    Text(Loc, String),
    Nat(Loc, usize),
    Bool(Loc, bool),
    Op(Loc, Box<Tm>, Op, Box<Tm>),
    // Doc(Loc, Box<Tm>, Doc),
    Var(Loc, String),
    Lam(Loc, String, Ty, Box<Tm>),
    App(Loc, Box<Tm>, Box<Tm>),
    Block(Loc, Vec<Tm>),
    Def(Loc, String, Box<Tm>),
}

impl Display for Tm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tm::Text(_, txt) => write!(f, "{:?}", txt),
            Tm::Nat(_, n) => write!(f, "{}", n),
            Tm::Bool(_, b) => write!(f, "{:?}", b),
            Tm::Op(_, x, op, y) => write!(f, "{} {} {}", x, op, y),
            Tm::Var(_, name) => write!(f, "{}", name),
            Tm::Lam(_, param, ty, body) => write!(f, "[fn {}: {} -> {}]", param, ty, body),
            Tm::App(_, func, arg) => write!(f, "[{} {}]", func, arg),
            Tm::Block(_, tms) => {
                writeln!(f, "{}", "{")?;
                for tm in tms {
                    writeln!(f, "    {}\n", tm)?;
                }
                writeln!(f, "{}", "}")?;
                Ok(())
            }
            Tm::Def(_, name, tm) => write!(f, "def {} = {}", name, tm),
        }
    }
}

impl Tm {
    pub fn loc(&self) -> &Loc {
        match self {
            Tm::Text(loc, _) => loc,
            Tm::Nat(loc, _) => loc,
            Tm::Bool(loc, _) => loc,
            Tm::Op(loc, _, _, _) => loc,
            Tm::Var(loc, _) => loc,
            Tm::Lam(loc, _, _, _) => loc,
            Tm::App(loc, _, _) => loc,
            Tm::Block(loc, _) => loc,
            Tm::Def(loc, _, _) => loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    SpaceConcat,
    Concat,
    Add,
    Sub,
    Mul,
    Div,
    Apply,
    And,
    Or,
    Lt,
    Lte,
    Gt,
    Gte,
    NatEq,
    TextEq,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::SpaceConcat => write!(f, "+++"),
            Op::Concat => write!(f, "++"),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Apply => write!(f, "$"),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Lt => write!(f, "lt"),
            Op::Lte => write!(f, "lte"),
            Op::Gt => write!(f, "gt"),
            Op::Gte => write!(f, "gte"),
            Op::NatEq => write!(f, "nat_eq"),
            Op::TextEq => write!(f, "text_eq"),
        }
    }
}

// #[derive(Debug)]
// pub enum Doc {}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Var(String),
    Arr(Box<Ty>, Box<Ty>),
    Builtin(BuiltinTy),
}

impl Ty {
    pub const fn void() -> Self {
        Ty::Builtin(BuiltinTy::Void)
    }

    pub const fn text() -> Self {
        Ty::Builtin(BuiltinTy::Text)
    }

    pub const fn nat() -> Self {
        Ty::Builtin(BuiltinTy::Nat)
    }

    pub const fn bool() -> Self {
        Ty::Builtin(BuiltinTy::Bool)
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Var(name) => write!(f, "{}", name),
            Ty::Arr(x, y) => write!(f, "[{} -> {}]", x, y),
            Ty::Builtin(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinTy {
    Void,
    Text,
    Nat,
    Bool,
}

impl Display for BuiltinTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinTy::Void => write!(f, "builtin::Void"),
            BuiltinTy::Text => write!(f, "builtin::Text"),
            BuiltinTy::Nat => write!(f, "builtin::Nat"),
            BuiltinTy::Bool => write!(f, "builtin::Bool"),
        }
    }
}
