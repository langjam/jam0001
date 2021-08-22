use ast2str::AstToStr;

use crate::{
    ast::{Expr, Stmt},
    data::{NativeObj, Value},
    eval::EvalError,
};

#[derive(Debug, Clone, AstToStr)]
pub enum Node<'s> {
    Expr(#[forward] Expr<'s>),
    Stmt(#[forward] Stmt<'s>),
    Empty,
}

impl<'s> From<Expr<'s>> for Node<'s> {
    fn from(e: Expr<'s>) -> Self {
        Self::Expr(e)
    }
}
impl<'s> From<Stmt<'s>> for Node<'s> {
    fn from(s: Stmt<'s>) -> Self {
        Self::Stmt(s)
    }
}
impl<'s, T> From<Option<T>> for Node<'s>
where
    T: Into<Node<'s>>,
{
    fn from(o: Option<T>) -> Self {
        o.map(|n| n.into()).unwrap_or(Self::Empty)
    }
}

#[derive(Debug)]
pub struct AstProxy<'s> {
    node: Node<'s>,
}

impl<'s> AstProxy<'s> {
    pub fn new<N: Into<Node<'s>>>(node: N) -> Self {
        Self { node: node.into() }
    }
}

impl<'s> NativeObj<'s> for AstProxy<'s> {
    fn to_string(&self) -> String {
        format!("<Node at {:p}>", self)
    }

    fn get_prop(&self, name: std::borrow::Cow<'s, str>) -> crate::eval::Signal<'s> {
        match &*name {
            "tree" => Value::Str(self.node.ast_to_str().into()).into(),
            _ => EvalError::UndefinedProperty(name).into(),
        }
    }

    fn set_prop(
        &self,
        name: std::borrow::Cow<'s, str>,
        value: crate::data::Value<'s>,
    ) -> crate::eval::Signal<'s> {
        todo!()
    }
}
