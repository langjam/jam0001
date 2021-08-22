use ast2str::AstToStr;

use crate::{
    ast::*,
    data::{NativeObj, Value},
    eval::{EvalError, Signal},
    stringify::Stringify,
};

#[derive(Debug, Clone, AstToStr)]
pub enum Node<'s> {
    Expr(#[forward] Expr<'s>),
    Stmt(#[forward] Stmt<'s>),
    Empty,
}

impl Stringify for Node<'_> {
    fn stringify_impl(&self, indent_level: usize) -> String {
        match self {
            Node::Expr(e) => e.stringify_impl(indent_level),
            Node::Stmt(s) => s.stringify_impl(indent_level),
            Node::Empty => String::new(),
        }
    }
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

macro_rules! parse {
    ($self:ident, binop $value:expr) => {{
        let value = $value.to_string();
        match value.trim() {
            "+" => BinOpKind::Add,
            "-" => BinOpKind::Sub,
            "*" => BinOpKind::Mul,
            "/" => BinOpKind::Div,
            "&&" => BinOpKind::And,
            "||" => BinOpKind::Or,
            "==" => BinOpKind::Eq,
            "!=" => BinOpKind::Ne,
            "<" => BinOpKind::Lt,
            "<=" => BinOpKind::Le,
            ">" => BinOpKind::Gt,
            ">=" => BinOpKind::Ge,
            _ => {
                return EvalError::SyntaxError(format!(
                    "Failed to parse the provided code as a binary operator:\n```\n{}\n```",
                    value
                ))
                .into()
            }
        }
    }};
    ($self:ident, unop $value:expr) => {{
        let value = $value.to_string();
        match value.trim() {
            "-" => UnOpKind::Neg,
            "!" => UnOpKind::Not,
            _ => {
                return EvalError::SyntaxError(format!(
                    "Failed to parse the provided code as a unary operator:\n```\n{}\n```",
                    value
                ))
                .into()
            }
        }
    }};
    ($self:ident, $what:ident, $value:expr) => {{
        let new_value = $self.arena.alloc($value.to_string());
        match $crate::parser::dank::$what(new_value) {
            Ok(expr) => expr,
            Err(e) => {
                return Self::syntax_error(stringify!($what), new_value, e);
            }
        }
    }};
}

#[derive(Debug)]
pub struct AstProxy<'s> {
    arena: &'s bumpalo::Bump,
    node: Node<'s>,
}

impl<'s> AstProxy<'s> {
    pub fn new<N: Into<Node<'s>>>(arena: &'s bumpalo::Bump, node: N) -> Self {
        Self {
            arena,
            node: node.into(),
        }
    }

    fn alloc<N: Into<Node<'s>>>(&self, n: N) -> Value<'s> {
        AstProxy::new(self.arena, n.into()).move_to_heap().into()
    }

    fn try_extract_prop(&self, name: std::borrow::Cow<'s, str>) -> Option<Value<'s>> {
        match &self.node {
            Node::Expr(expr) => self.extract_expr_prop(expr, name),
            Node::Stmt(stmt) => self.extract_stmt_prop(stmt, name),
            Node::Empty => todo!(),
        }
    }

    fn try_update_prop(&self, name: std::borrow::Cow<'s, str>, value: Value<'s>) -> Signal<'s> {
        match &self.node {
            Node::Expr(expr) => self.update_expr_prop(expr, name, value),
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
                let new_expr = parse!(self, expr, value);
                *initializer = Some(Box::new(new_expr));
            }
            StmtKind::FuncDecl(func) if ["name", "args", "body"].contains(&&*name) => {
                self.update_func_prop(func, name, value)?;
            }
            StmtKind::ExprStmt(_) => todo!(),
            StmtKind::Print(_) => todo!(),
            StmtKind::Block(_) => todo!(),
            StmtKind::UnscopedBlock(_) => todo!(),
            StmtKind::If(_) => todo!(),
            StmtKind::While(_, _) => todo!(),
            StmtKind::Return(ret_value) if name == "value" => {
                let new_expr = parse!(self, expr, value);
                *ret_value = Some(Box::new(new_expr));
            }
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
                    Some(node) => Some(self.alloc(*node)),
                    None => Some(Value::Null),
                }
            }
            StmtKind::Return(value) if name == "value" => match value.as_ref().cloned() {
                Some(node) => Some(self.alloc(*node)),
                None => Some(Value::Null),
            },
            StmtKind::FuncDecl(func) if ["name", "args", "body"].contains(&&*name) => {
                self.extract_func_prop(func, name).into()
            }
            StmtKind::ExprStmt(_) => todo!(),
            StmtKind::Print(_) => todo!(),
            StmtKind::Block(_) => todo!(),
            StmtKind::UnscopedBlock(_) => todo!(),
            StmtKind::If(_) => todo!(),
            StmtKind::While(_, _) => todo!(),
            StmtKind::Assignment(_, _) => todo!(),
            StmtKind::PropAssignment(_, _, _) => todo!(),
            StmtKind::Break | StmtKind::Continue => None,
            _ => None,
        }
    }

    fn update_expr_prop(
        &self,
        stmt: &Expr<'s>,
        name: std::borrow::Cow<'s, str>,
        value: Value<'s>,
    ) -> Signal<'s> {
        match &mut *stmt.kind.borrow_mut() {
            ExprKind::Literal(l) if name == "value" => *l = parse!(self, literal, value),
            ExprKind::Variable(v) if name == "name" => *v = value.to_string().into(),
            ExprKind::Property(prop_name, _) if name == "name" => {
                *prop_name = value.to_string().into()
            }
            ExprKind::Property(_, object) if name == "object" => {
                **object = parse!(self, expr, value)
            }
            ExprKind::Binary(l, _, _) if name == "left" => **l = parse!(self, expr, value),
            ExprKind::Binary(_, op, _) if name == "op" => *op = parse!(self, binop value),
            ExprKind::Binary(_, _, r) if name == "right" => **r = parse!(self, expr, value),
            ExprKind::Unary(op, _) if name == "op" => *op = parse!(self, unop value),
            ExprKind::Unary(_, operand) if name == "operand" => {
                **operand = parse!(self, expr, value)
            }
            ExprKind::Call(callee, _) if name == "callee" => **callee = parse!(self, expr, value),
            ExprKind::ObjectLiteral(_) => todo!(),
            ExprKind::LambdaLiteral(_) => todo!(),
            rest => {
                return EvalError::UndefinedProperty(
                    format!(
                        "Property `{}` doesn't exist on a node of type `{:?}`",
                        name,
                        ExprKindName::from(&*rest)
                    )
                    .into(),
                )
                .into()
            }
        }
        Value::Null.into()
    }

    fn extract_expr_prop(
        &self,
        expr: &Expr<'s>,
        name: std::borrow::Cow<'s, str>,
    ) -> Option<Value<'s>> {
        match &*expr.kind.borrow() {
            ExprKind::Variable(v) if name == "name" => Some(Value::Str(v.clone())),
            ExprKind::Literal(v) if name == "value" => Some(v.clone()),
            ExprKind::Property(name, _) if name == "name" => Some(Value::Str(name.clone())),
            ExprKind::Property(_, object) if name == "object" => Some(self.alloc(*object.clone())),
            ExprKind::Binary(left, _, _) if name == "left" => Some(self.alloc(*left.clone())),
            ExprKind::Binary(_, _, right) if name == "right" => Some(self.alloc(*right.clone())),
            ExprKind::Binary(_, op, _) if name == "op" => Some(Value::Str(op.symbol().into())),
            ExprKind::Unary(op, _) if name == "op" => Some(Value::Str(op.symbol().into())),
            ExprKind::Unary(_, operand) if name == "operand" => Some(self.alloc(*operand.clone())),
            ExprKind::Call(callee, _) if name == "callee" => Some(self.alloc(*callee.clone())),
            ExprKind::Call(_, _args) if name == "args" => todo!(),
            ExprKind::ObjectLiteral(_) => todo!(),
            ExprKind::LambdaLiteral(_) => todo!(),
            _ => None,
        }
    }

    fn extract_func_prop(&self, func: &Function<'s>, name: std::borrow::Cow<'s, str>) -> Value<'s> {
        match &*name {
            "name" => Value::Str(func.name.clone()),
            "args" => todo!(),
            "body" => self.alloc(func.body.clone()),
            _ => unreachable!(),
        }
    }

    fn update_func_prop(
        &self,
        func: &mut Function<'s>,
        name: std::borrow::Cow<'s, str>,
        value: Value<'s>,
    ) -> Signal<'s> {
        match &*name {
            "name" => func.name = value.to_string().into(),
            "args" => todo!(),
            "body" => {
                func.body = parse!(self, block, value);
            }
            _ => unreachable!(),
        }
        Value::Null.into()
    }

    fn syntax_error(
        what: &str,
        source: &str,
        e: peg::error::ParseError<peg::str::LineCol>,
    ) -> Signal<'s> {
        EvalError::SyntaxError(format!(
            "Failed to parse the provided code as {}:\n```\n{}\n```\n{}",
            what, source, e
        ))
        .into()
    }
}

impl<'s> NativeObj<'s> for AstProxy<'s> {
    fn to_string(&self) -> String {
        format!("<Node at {:p}>", self)
    }

    fn get_prop(&self, name: std::borrow::Cow<'s, str>) -> crate::eval::Signal<'s> {
        match &*name {
            "tree" => Value::Str(self.node.ast_to_str().into()).into(),
            "code" => Value::Str(self.node.stringify().into()).into(),
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

pub fn fmt_impl<'s, 'b: 's>(arena: &'b bumpalo::Bump, args: Vec<Value<'s>>) -> Signal<'s> {
    use dynfmt::{Format, SimpleCurlyFormat};

    if args.is_empty() {
        return Value::Str("".into()).into();
    }

    let fmt = arena.alloc(args[0].to_string());
    let args = args[1..].iter().map(|a| a.to_string()).collect::<Vec<_>>();
    match SimpleCurlyFormat.format(&*fmt, args) {
        Ok(s) => Value::Str(s).into(),
        Err(_) => todo!(),
    }
}
