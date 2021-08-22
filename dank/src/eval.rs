use std::borrow::Cow;

use crate::ast::*;
use crate::ast_proxy::AstProxy;
use crate::{
    data::{NativeObj, Object, Value},
    env::Env,
};

#[derive(Debug)]
pub struct Evaluator<'a, 'b: 'a> {
    source: &'a str,
    buf_stack: Vec<String>,
    env: Env<Cow<'a, str>, Value<'a>>,
    arena: &'b bumpalo::Bump,
    // How deep are we inside commented code. Used to introduce global comment-time scoping.
    comment_depth: usize,
    is_in_loop: bool,
}

#[derive(Debug, Clone)]
pub enum EvalError<'a> {
    ArityMismatch(String),
    NotCallable(String),
    TypeError(String),
    RuntimeError(String),
    SyntaxError(String),
    UndefinedProperty(Cow<'a, str>),
    DuplicatedLiteralField(Cow<'a, str>),
    UndefinedVariable(Cow<'a, str>),
}

#[derive(Debug, Clone)]
pub enum Signal<'a> {
    Value(Value<'a>),
    Return(Value<'a>),
    Break,
    Continue,
    Error(EvalError<'a>),
}

impl<'a> std::ops::FromResidual for Signal<'a> {
    fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
        residual
    }
}

impl<'a> std::ops::Try for Signal<'a> {
    type Output = Value<'a>;
    type Residual = Signal<'a>;

    fn from_output(output: Self::Output) -> Self {
        Self::Value(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Signal::Value(value) => std::ops::ControlFlow::Continue(value),
            Signal::Return(_) | Signal::Break | Signal::Continue | Signal::Error(_) => {
                std::ops::ControlFlow::Break(self)
            }
        }
    }
}

impl<'a> From<Value<'a>> for Signal<'a> {
    fn from(v: Value<'a>) -> Self {
        Self::Value(v)
    }
}

impl<'a> From<EvalError<'a>> for Signal<'a> {
    fn from(e: EvalError<'a>) -> Self {
        Self::Error(e)
    }
}

impl<'a> From<Result<Value<'a>, EvalError<'a>>> for Signal<'a> {
    fn from(res: Result<Value<'a>, EvalError<'a>>) -> Self {
        match res {
            Ok(ok) => ok.into(),
            Err(e) => e.into(),
        }
    }
}

impl<'a> Signal<'a> {
    #[inline]
    pub fn extract(self) -> Option<Value<'a>> {
        match self {
            Signal::Value(v) | Signal::Return(v) => Some(v),
            Signal::Break | Signal::Continue | Signal::Error(_) => None,
        }
    }

    #[inline]
    pub fn extract_unwrap(self) -> Value<'a> {
        self.extract()
            .expect("Attempted to extract a value from a break or continue signal.")
    }

    #[inline]
    pub fn map_return(self) -> Self {
        match self {
            Signal::Return(v) => Signal::Value(v),
            _ => self,
        }
    }
}

// TODO: return/break/continue signals
impl<'a, 'b> Evaluator<'a, 'b> {
    pub fn with_env(
        env: Env<Cow<'a, str>, Value<'a>>,
        source: &'a str,
        arena: &'b bumpalo::Bump,
    ) -> Self {
        Self {
            source,
            env,
            buf_stack: Vec::new(),
            comment_depth: 0,
            is_in_loop: false,
            arena,
        }
    }

    pub fn eval(&mut self, ast: &mut Ast<'a>) -> Result<(), EvalError<'a>> {
        self.eval_comments(ast)?;
        self.eval_ast(ast)?;
        Ok(())
    }

    fn eval_comments(&mut self, ast: &mut Ast<'a>) -> Result<(), EvalError<'a>> {
        self.eval_comments_in_block(&mut ast.statements);
        Ok(())
    }

    fn eval_comments_in_block(&mut self, block: &mut [LineComment<'a>]) {
        self.env.push();
        self.comment_depth += 1;

        for line in block {
            // TODO: allow comments to actually modify the next statement
            let replacement = if let CommentBody::Stmt(ref mut stmt) = line.body {
                // NOTE: We do not evaluate nested comments.
                // QQQ: how should we handle errors occurring at comment-time?
                self.buf_stack.push(String::new());

                let mut previous_ast = self.env.get(&"AST".into()).cloned();

                self.env.add(
                    "AST".into(),
                    AstProxy::new(self.arena, line.stmt.clone())
                        .move_to_heap()
                        .into(),
                );

                let is_success = match self.eval_stmt(stmt) {
                    Signal::Error(e) => {
                        // TODO: optionally disable this
                        eprintln!("[Evaluator] Failed to evaluate a comment: {:?}", e);
                        None
                    }
                    _ => Some(()),
                };

                self.env.remove(&"AST".into());
                if let Some(value) = previous_ast.take() {
                    self.env.update_or_add("AST".into(), value);
                }

                let code = self.arena.alloc(self.buf_stack.pop().unwrap()).trim();
                let code = if code.is_empty() { None } else { Some(code) };
                is_success.and(code).and_then(move |code| {
                    crate::parser::grammar::file(code)
                        .map(|ast| Stmt {
                            span: 0..0,
                            kind: StmtKind::UnscopedBlock(ast.statements).alloc(),
                        })
                        .map_err(|e| {
                            eprintln!("The printed code wasn't valid, ignoring it: {:?}. Code was:\n```\n{}\n```", e, code);
                        })
                        .ok()
                })
            } else {
                None
            };

            if let Some(stmt) = line.stmt.as_mut() {
                self.eval_comments_in_stmt(stmt);
            }

            if let Some(stmt) = replacement {
                line.stmt = Some(stmt);
            }
        }

        self.comment_depth -= 1;
        self.env.pop();
    }

    fn eval_comments_in_stmt(&mut self, stmt: &mut Stmt<'a>) {
        match &mut *stmt.kind.borrow_mut() {
            StmtKind::LetDecl(_, init) => init
                .as_mut()
                .map(|e| self.eval_comments_in_expr(e))
                .unwrap_or(()),
            StmtKind::FuncDecl(f) => {
                self.eval_comments_in_stmt(&mut f.body);
            }
            StmtKind::ExprStmt(expr) => self.eval_comments_in_expr(expr),
            StmtKind::Print(args) => args.iter_mut().for_each(|e| self.eval_comments_in_expr(e)),
            StmtKind::Block(b) => self.eval_comments_in_block(&mut b.statements),
            StmtKind::While(_, stmt) => self.eval_comments_in_stmt(stmt),
            StmtKind::If(r#if) => {
                r#if.branches.iter_mut().for_each(|(c, b)| {
                    self.eval_comments_in_expr(c);
                    self.eval_comments_in_stmt(b);
                });
                if let Some(b) = r#if.otherwise.as_mut() {
                    self.eval_comments_in_stmt(b)
                }
            }
            StmtKind::UnscopedBlock(b) => {
                self.eval_comments_in_block(b);
            }
            StmtKind::Return(v) => v
                .as_mut()
                .map(|v| self.eval_comments_in_expr(v))
                .unwrap_or(()),
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Assignment(_, value) => self.eval_comments_in_expr(value),
            StmtKind::PropAssignment(obj, _, value) => {
                self.eval_comments_in_expr(obj);
                self.eval_comments_in_expr(value);
            }
            StmtKind::DynPropAssignment(obj, key, value) => {
                self.eval_comments_in_expr(obj);
                self.eval_comments_in_expr(key);
                self.eval_comments_in_expr(value);
            }
        }
    }

    fn eval_comments_in_expr(&mut self, expr: &mut Expr<'a>) {
        match &mut *expr.kind.borrow_mut() {
            ExprKind::ObjectLiteral(pairs) => pairs
                .iter_mut()
                .for_each(|(_, e)| self.eval_comments_in_expr(e)),
            ExprKind::LambdaLiteral(f) => {
                self.eval_comments_in_stmt(&mut f.body);
            }
            ExprKind::Binary(l, _, r) => {
                self.eval_comments_in_expr(l);
                self.eval_comments_in_expr(r);
            }
            ExprKind::Unary(_, operand) => {
                self.eval_comments_in_expr(operand);
            }
            ExprKind::Call(callee, args) => {
                self.eval_comments_in_expr(callee);
                args.iter_mut().for_each(|a| self.eval_comments_in_expr(a));
            }
            ExprKind::Literal(_) => (),
            ExprKind::Variable(_) => (),
            ExprKind::Property(_, obj) => {
                self.eval_comments_in_expr(obj);
            }
            ExprKind::DynProperty(key, obj) => {
                self.eval_comments_in_expr(obj);
                self.eval_comments_in_expr(key);
            }
        }
    }

    fn eval_ast(&mut self, ast: &Ast<'a>) -> Result<Value<'a>, EvalError<'a>> {
        match self.eval_block(&ast.statements) {
            Signal::Error(e) => Err(e),
            rest => Ok(rest.extract_unwrap()),
        }
    }

    fn eval_block(&mut self, block: &[LineComment<'a>]) -> Signal<'a> {
        // comment_depth == 1 implies global comment scope
        if self.comment_depth != 1 {
            self.env.push();
        }
        let result = self.eval_unscoped_block(block);
        if self.comment_depth != 1 {
            self.env.pop();
        }
        result
    }

    fn eval_unscoped_block(&mut self, block: &[LineComment<'a>]) -> Signal<'a> {
        for stmt in block.iter().filter_map(|line| line.stmt.as_ref()) {
            self.eval_stmt(stmt)?;
        }
        Value::Null.into()
    }

    fn eval_expr(&mut self, expr: &Expr<'a>) -> Signal<'a> {
        match &*expr.kind.borrow() {
            ExprKind::Literal(l) => l.clone().into(),
            ExprKind::Variable(v) => match self.env.get(v).cloned() {
                Some(v) => v.into(),
                None => EvalError::UndefinedVariable(v.clone()).into(),
            },
            ExprKind::ObjectLiteral(lit) => {
                let mut object = Object::default();
                for (k, v) in lit {
                    let value = self.eval_expr(v)?;
                    if object.fields.contains_key(k) {
                        return EvalError::DuplicatedLiteralField(k.clone()).into();
                    }
                    object.fields.insert(k.clone(), value);
                }
                Value::Obj(object.move_on_heap()).into()
            }
            ExprKind::LambdaLiteral(f) => {
                let fn_ref = crate::data::Ptr::new((&**f).clone());
                Value::Fn(fn_ref).into()
            }
            ExprKind::Binary(l, op, r) => {
                macro_rules! bin_op {
                    ($self:ident, $left:ident $op:tt $right:ident) => {{
                        let left = $self.eval_expr($left)?;
                        let right = $self.eval_expr($right)?;
                        left $op right
                    }};
                }
                match op {
                    BinOpKind::Add => bin_op!(self, l + r),
                    BinOpKind::Sub => bin_op!(self, l - r),
                    BinOpKind::Mul => bin_op!(self, l * r),
                    BinOpKind::Div => bin_op!(self, l / r),
                    BinOpKind::Eq => Value::from(bin_op!(self, l == r)).into(),
                    BinOpKind::Ne => Value::from(bin_op!(self, l != r)).into(),
                    BinOpKind::Lt => Value::from(bin_op!(self, l < r)).into(),
                    BinOpKind::Le => Value::from(bin_op!(self, l <= r)).into(),
                    BinOpKind::Gt => Value::from(bin_op!(self, l > r)).into(),
                    BinOpKind::Ge => Value::from(bin_op!(self, l >= r)).into(),
                    BinOpKind::Or => {
                        let l = self.eval_expr(l)?;
                        if self.is_truthy(&l) {
                            l.into()
                        } else {
                            self.eval_expr(r)
                        }
                    }
                    BinOpKind::And => {
                        let l = self.eval_expr(l)?;
                        let r = self.eval_expr(r)?;
                        if !self.is_truthy(&l) {
                            l.into()
                        } else {
                            r.into()
                        }
                    }
                }
            }
            ExprKind::Unary(op, operand) => {
                let operand = self.eval_expr(operand)?;
                match op {
                    UnOpKind::Not => Value::Bool(!self.is_truthy(&operand)).into(),
                    UnOpKind::Neg if matches!(operand, Value::Num(_)) => {
                        let value = match operand {
                            Value::Num(n) => n,
                            _ => unreachable!(),
                        };
                        Value::Num(-value).into()
                    }
                    UnOpKind::Neg => EvalError::TypeError(format!(
                        "Cannot apply the operator `-` to `{}`",
                        operand
                    ))
                    .into(),
                }
            }
            ExprKind::Property(name, obj) => {
                let obj = self.eval_expr(obj)?;
                self.get_prop(&obj, name.clone())
            }
            ExprKind::DynProperty(name, obj) => {
                let obj = self.eval_expr(obj)?;
                let name = self.eval_expr(name)?.to_string();
                self.get_prop(&obj, name.into())
            }
            ExprKind::Call(callee, args) => {
                let callee = self.eval_expr(callee)?;
                let mut arg_values = vec![];
                for a in args {
                    arg_values.push(self.eval_expr(a)?);
                }

                self.call(&callee, arg_values)
            }
        }
    }

    fn call(&mut self, callee: &Value<'a>, args: Vec<Value<'a>>) -> Signal<'a> {
        let previous_state = self.is_in_loop;
        self.is_in_loop = false;
        let result = (|| match callee {
            Value::Fn(func) => {
                if func.args.len() != args.len() {
                    return EvalError::ArityMismatch(format!(
                        "{} expected {} arguments but was given {}",
                        callee,
                        func.args.len(),
                        args.len()
                    ))
                    .into();
                }

                self.env.push();
                for (arg, arg_value) in func.args.iter().zip(args.into_iter()) {
                    self.env.add(arg.clone(), arg_value);
                }
                let result = self.eval_stmt(&func.body);
                let _ = self.env.pop();
                result.map_return()
            }
            Value::NativeFn(func) => {
                if func.arity >= 0 && func.arity as usize != args.len() {
                    return EvalError::ArityMismatch(format!(
                        "{} expected {} arguments but was given {}",
                        callee,
                        func.arity,
                        args.len()
                    ))
                    .into();
                }

                (func.func)(args).map_return()
            }
            Value::Obj(_) | Value::NativeObj(_) => {
                match self.get_prop(callee, "__call__".into()).extract() {
                    Some(method) => self.call(&method, args),
                    None => EvalError::NotCallable(format!(
                        "Object {} is missing the __call__ property so it cannot be called",
                        callee
                    ))
                    .into(),
                }
            }
            Value::Num(_) | Value::Bool(_) | Value::Null | Value::Str(_) => {
                EvalError::NotCallable(format!("`{}` is not a callable object.", callee)).into()
            }
        })();
        self.is_in_loop = previous_state;
        result
    }

    fn get_prop(&mut self, obj: &Value<'a>, name: Cow<'a, str>) -> Signal<'a> {
        match obj {
            Value::Obj(obj) => {
                let obj = obj.borrow();
                obj.fields
                    .get(&name)
                    .cloned()
                    .ok_or(EvalError::UndefinedProperty(name))
                    .into()
            }
            Value::NativeObj(obj) => obj.get_prop(name),
            Value::Str(s) if name == "length" => Value::Num(s.len() as _).into(),
            _ => EvalError::UndefinedProperty(name).into(),
        }
    }

    fn set_prop(&mut self, obj: &Value<'a>, name: Cow<'a, str>, value: Value<'a>) -> Signal<'a> {
        match obj {
            Value::Obj(obj) => {
                let mut obj = obj.borrow_mut();
                obj.fields.insert(name, value.clone());
                value.into()
            }
            Value::NativeObj(obj) => obj.set_prop(name, value),
            _ => EvalError::UndefinedProperty(name).into(),
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt<'a>) -> Signal<'a> {
        match &*stmt.kind.borrow() {
            StmtKind::Print(args) => {
                for (i, a) in args.iter().enumerate() {
                    let value = self.eval_expr(a)?;
                    self.print(format!("{}", value));
                    if i != args.len() - 1 {
                        self.print(" ");
                    }
                }
                self.print("\n");
            }
            StmtKind::LetDecl(name, initializer) => {
                let value = if let Some(i) = initializer {
                    self.eval_expr(i)?
                } else {
                    Value::Null
                };
                self.env.add(name.clone(), value);
            }
            StmtKind::FuncDecl(f) => {
                let fn_ref = crate::data::Ptr::new((&**f).clone());
                let fn_obj = Value::Fn(fn_ref);
                self.env.add(f.name.clone(), fn_obj);
            }
            StmtKind::Assignment(name, value) => {
                let value = self.eval_expr(value)?;
                if self.env.update(name, value).is_none() {
                    return EvalError::UndefinedVariable(name.clone()).into();
                }
            }
            StmtKind::PropAssignment(obj, prop, value) => {
                let obj = self.eval_expr(obj)?;
                let value = self.eval_expr(value)?;
                self.set_prop(&obj, prop.clone(), value)?;
            }
            StmtKind::DynPropAssignment(obj, key, value) => {
                let obj = self.eval_expr(obj)?;
                let key = self.eval_expr(key)?.to_string();
                let value = self.eval_expr(value)?;
                self.set_prop(&obj, key.into(), value);
            }
            StmtKind::Block(b) => return self.eval_block(&b.statements),
            StmtKind::UnscopedBlock(b) => return self.eval_unscoped_block(b),
            StmtKind::ExprStmt(e) => {
                self.eval_expr(e)?;
            }
            StmtKind::While(condition, body) => {
                let previous_state = self.is_in_loop;
                self.is_in_loop = true;
                let signal = loop {
                    let condition = self.eval_expr(condition)?;
                    if !self.is_truthy(&condition) {
                        break Value::Null.into();
                    }
                    match self.eval_stmt(body) {
                        // Handle break and continue signals explicitly, while propagating returns and errors
                        Signal::Break => break Value::Null.into(),
                        Signal::Continue => (),
                        Signal::Return(v) => break Signal::Return(v),
                        Signal::Error(e) => break e.into(),
                        // Ignore normal values
                        Signal::Value(_) => (),
                    }
                };
                self.is_in_loop = previous_state;
                return signal;
            }
            StmtKind::If(r#if) => {
                for (condition, body) in &r#if.branches {
                    let condition = self.eval_expr(condition)?;
                    if self.is_truthy(&condition) {
                        return self.eval_stmt(body);
                    }
                }
                if let Some(r#else) = r#if.otherwise.as_ref() {
                    return self.eval_stmt(r#else);
                }
            }
            StmtKind::Break => return self.generate_loop_signal(Signal::Break),
            StmtKind::Continue => return self.generate_loop_signal(Signal::Continue),
            StmtKind::Return(v) => {
                return if let Some(v) = v.as_ref() {
                    Signal::Return(self.eval_expr(v)?)
                } else {
                    Signal::Return(Value::Null)
                };
            }
        }
        Value::Null.into()
    }

    fn print<S: Into<String>>(&mut self, s: S) {
        let s = s.into();
        if self.is_processing_comments() {
            self.buf_stack.last_mut().unwrap().push_str(&s);
        } else {
            print!("{}", s)
        }
    }

    #[inline]
    fn is_processing_comments(&self) -> bool {
        self.comment_depth > 0
    }

    fn is_truthy(&self, value: &Value<'a>) -> bool {
        match value {
            Value::Num(n) => *n != 0.0,
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::Str(_) => true,
            Value::Obj(_) => true,
            Value::Fn(_) => true,
            Value::NativeFn(_) => true,
            Value::NativeObj(_) => true,
        }
    }

    pub fn bin_op_type_error(l: &Value<'a>, r: &Value<'a>, op: &str) -> EvalError<'a> {
        EvalError::TypeError(format!(
            "Cannot apply the operator `{}` to `{}` and `{}`",
            op, l, r,
        ))
    }

    fn generate_loop_signal(&self, signal: Signal<'a>) -> Signal<'a> {
        let noun = match signal {
            Signal::Break => "break",
            Signal::Continue => "continue",
            Signal::Error(_) => unreachable!(),
            Signal::Value(_) => unreachable!(),
            Signal::Return(_) => unreachable!(),
        };

        if self.is_in_loop {
            signal
        } else {
            EvalError::RuntimeError(format!("Cannot {} outside of a loop", noun)).into()
        }
    }
}

#[cfg(test)]
mod tests {}
