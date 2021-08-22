use std::fmt::{Display, Formatter};
use crate::parsing::ast::{Stmt};
use crate::parsing::token::Token;
use crate::execution::predef::RcNative;

#[derive(Clone)]
pub enum Object {
    String(String),
    Num(i64),
    Function(Vec<Stmt>, Vec<Token>),
    NativeFunction(RcNative, usize)
}

impl Object {
    pub fn is_true(&self) -> bool {
        match self {
            Object::String(s) => {s.is_empty()}
            Object::Num(n) => {*n!=0}
            Object::Function(_, _) => {true}
            Object::NativeFunction(_, _) => {true}
        }
    }

    pub fn unwrap_num(&self) -> Option<i64> {
        match self {
            Object::Num(n) => {Some(*n)},
            _ => None
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        if std::mem::discriminant(self)!=std::mem::discriminant(other){
            return false;
        }
        use Object::*;

        match (self, other) {
            (String(s1), String(s2)) => {s1==s2},
            (Num(n1), Num(n2)) => {n1==n2},
            (Function(f1,arg1), Function(f2,arg2))
                => {f1==f2 && arg1==arg2}
            (NativeFunction(f1, a1), NativeFunction(f2, a2))
                => {
                a1==a2 && std::ptr::eq(f1, f2)
            }
            _ => panic!("unimplemented object comparison")
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => {write!(f, "{}", s)}
            Object::Num(n) => {write!(f, "{}", n)}
            Object::Function(_, args) => {
                write!(f, "function with {} arg(s)", args.len())
            }
            Object::NativeFunction(_, arity) => {
                write!(f, "native function with {} arg(s)", arity)
            }
        }

    }
}