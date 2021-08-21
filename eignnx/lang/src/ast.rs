use std::fmt::Display;

use crate::tcx::Tcx;

#[derive(Debug, Clone)]
pub struct Loc;

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "file:? ?:?")
    }
}

#[derive(Debug, Clone)]
pub enum Tm {
    Text(Loc, String),
    // Doc(Loc, Box<Tm>, Doc),
    Var(Loc, String),
    Lam(Loc, String, Ty, Box<Tm>),
    App(Loc, Box<Tm>, Box<Tm>),
    TyLam(Loc, String, Box<Tm>),
    TyApp(Loc, Box<Tm>, Ty),
}

impl Display for Tm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tm::Text(_, txt) => write!(f, "{:?}", txt),
            Tm::Var(_, name) => write!(f, "{}", name),
            Tm::Lam(_, param, ty, body) => write!(f, "[fn {}: {} -> {}]", param, ty, body),
            Tm::App(_, func, arg) => write!(f, "[{} {}]", func, arg),
            Tm::TyLam(_, ty_var, body) => {
                write!(f, "tyfn {} -> {}", ty_var, body)
            }
            Tm::TyApp(_, ty_func, ty_arg) => write!(f, "{{{} {}}}", ty_func, ty_arg),
        }
    }
}

impl Tm {
    pub fn loc(&self) -> &Loc {
        match self {
            Tm::Text(loc, _) => loc,
            Tm::Var(loc, _) => loc,
            Tm::Lam(loc, _, _, _) => loc,
            Tm::App(loc, _, _) => loc,
            Tm::TyLam(loc, _, _) => loc,
            Tm::TyApp(loc, _, _) => loc,
        }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum Kind {
//     Star,
//     Arr(Box<Kind>, Box<Kind>),
// }

// impl Display for Kind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Kind::Star => write!(f, "{}", "*"),
//             Kind::Arr(k1, k2) => write!(f, "({} => {})", k1, k2),
//         }
//     }
// }

// #[derive(Debug)]
// pub enum Doc {}

#[derive(Debug, Clone)]
pub enum Ty {
    Var(String),
    Arr(Box<Ty>, Box<Ty>),
    ForAll(Tcx, String, Box<Ty>),
    Builtin(BuiltinTy),
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(l0), Self::Var(r0)) => l0 == r0,
            (Self::Arr(l0, l1), Self::Arr(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::ForAll(_, l1, l2), Self::ForAll(_, r1, r2)) => l1 == r1 && l2 == r2,
            (Self::Builtin(l0), Self::Builtin(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Var(name) => write!(f, "{}", name),
            Ty::Arr(x, y) => write!(f, "[{} -> {}]", x, y),
            Ty::ForAll(_tcx, var, body) => write!(f, "[any {}. {}]", var, body),
            Ty::Builtin(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinTy {
    Text,
}

impl Display for BuiltinTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinTy::Text => write!(f, "{}", "builtin::Text"),
        }
    }
}
