use ast2str::AstToStr;

use crate::{
    ast::{Expr, ExprKind, Stmt, StmtKind, StmtKindName},
    data::{NativeObj, Value},
    eval::{EvalError, Signal},
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

fn alloc<'s, N: Into<Node<'s>>>(n: N) -> Value<'s> {
    AstProxy::new(n.into()).move_to_heap().into()
}

impl<'s> AstProxy<'s> {
    pub fn new<N: Into<Node<'s>>>(node: N) -> Self {
        Self { node: node.into() }
    }

    fn try_extract_prop(&self, name: std::borrow::Cow<'s, str>) -> Option<Value<'s>> {
        match &self.node {
            Node::Expr(_) => todo!(),
            Node::Stmt(stmt) => self.extract_stmt_prop(stmt, name),
            Node::Empty => todo!(),
        }
    }

    fn try_update_prop(&self, name: std::borrow::Cow<'s, str>, value: Value<'s>) -> Signal<'s> {
        match &self.node {
            Node::Expr(_) => todo!(),
            Node::Stmt(stmt) => self.update_stmt_prop(stmt, name, value),
            Node::Empty => EvalError::UndefinedProperty(
                "Attempted to set a property on a removed node.".into(),
            )
            .into(),
        }
    }

    fn update_stmt_prop(
        &self,
        stmt: &Stmt<'s>,
        name: std::borrow::Cow<'s, str>,
        value: Value<'s>,
    ) -> Signal<'s> {
        match &mut *stmt.kind.borrow_mut() {
            StmtKind::LetDecl(binding_name, _) if name == "name" => {
                *binding_name = value.to_string().into();
            }
            StmtKind::LetDecl(_, initializer) if name == "initializer" => {
                todo!()
            }
            StmtKind::FuncDecl(_) => todo!(),
            StmtKind::ExprStmt(_) => todo!(),
            StmtKind::Print(_) => todo!(),
            StmtKind::Block(_) => todo!(),
            StmtKind::UnscopedBlock(_) => todo!(),
            StmtKind::If(_) => todo!(),
            StmtKind::While(_, _) => todo!(),
            StmtKind::Return(_) => todo!(),
            StmtKind::Assignment(_, _) => todo!(),
            StmtKind::PropAssignment(_, _, _) => todo!(),
            StmtKind::Break => todo!(),
            StmtKind::Continue => todo!(),
            rest => {
                return EvalError::UndefinedProperty(
                    format!(
                        "Property `{}` doesn't exist on a node of type `{:?}`",
                        name,
                        StmtKindName::from(&*rest)
                    )
                    .into(),
                )
                .into()
            }
        }
        Value::Null.into()
    }

    fn extract_stmt_prop(
        &self,
        stmt: &Stmt<'s>,
        name: std::borrow::Cow<'s, str>,
    ) -> Option<Value<'s>> {
        match &*stmt.kind.borrow() {
            StmtKind::LetDecl(binding_name, _) if name == "name" => {
                Some(Value::Str(binding_name.clone()))
            }
            StmtKind::LetDecl(_, initializer) if name == "initializer" => {
                match initializer.as_ref().cloned() {
                    Some(node) => Some(alloc(*node)),
                    None => Some(Value::Null),
                }
            }
            StmtKind::FuncDecl(_) => todo!(),
            StmtKind::ExprStmt(_) => todo!(),
            StmtKind::Print(_) => todo!(),
            StmtKind::Block(_) => todo!(),
            StmtKind::UnscopedBlock(_) => todo!(),
            StmtKind::If(_) => todo!(),
            StmtKind::While(_, _) => todo!(),
            StmtKind::Return(_) => todo!(),
            StmtKind::Assignment(_, _) => todo!(),
            StmtKind::PropAssignment(_, _, _) => todo!(),
            StmtKind::Break | StmtKind::Continue => None,
            _ => None,
        }
    }

    fn extract_expr_prop(
        &self,
        expr: &Expr<'s>,
        name: std::borrow::Cow<'s, str>,
    ) -> Option<Value<'s>> {
        todo!();
        let kind_ref = expr.kind.clone();
        match &*expr.kind.borrow() {
            ExprKind::ObjectLiteral(fields) => {}
            ExprKind::LambdaLiteral(_) => todo!(),
            ExprKind::Literal(_) => todo!(),
            ExprKind::Variable(_) => todo!(),
            ExprKind::Property(_, _) => todo!(),
            ExprKind::Call(_, _) => todo!(),
            ExprKind::Binary(_, _, _) => todo!(),
            ExprKind::Unary(_, _) => todo!(),
        }

        None
    }
}

impl<'s> NativeObj<'s> for AstProxy<'s> {
    fn to_string(&self) -> String {
        format!("<Node at {:p}>", self)
    }

    fn get_prop(&self, name: std::borrow::Cow<'s, str>) -> crate::eval::Signal<'s> {
        match &*name {
            "tree" => Value::Str(self.node.ast_to_str().into()).into(),
            _ => self
                .try_extract_prop(name.clone())
                .map(Into::into)
                .unwrap_or_else(|| EvalError::UndefinedProperty(name).into()),
        }
    }

    fn set_prop(
        &self,
        name: std::borrow::Cow<'s, str>,
        value: crate::data::Value<'s>,
    ) -> crate::eval::Signal<'s> {
        self.try_update_prop(name, value)
    }
}
