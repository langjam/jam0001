use std::borrow::Cow;

use crate::ast::*;
use crate::data::Object;
use crate::{data::Value, env::Env};

#[derive(Debug)]
pub struct Evaluator<'a, 'b: 'a> {
    buf_stack: Vec<String>,
    env: Env<Cow<'a, str>, Value<'a>>,
    is_processing_comments: bool,
    is_in_loop: bool,
    arena: &'b bumpalo::Bump,
}

#[derive(Debug, Clone)]
pub enum EvalError<'a> {
    ArityMismatch(String),
    NotCallable(String),
    TypeError(String),
    RuntimeError(String),
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
    pub fn with_env(env: Env<Cow<'a, str>, Value<'a>>, arena: &'b bumpalo::Bump) -> Self {
        Self {
            env,
            buf_stack: Vec::new(),
            is_processing_comments: false,
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
        self.is_processing_comments = true;
        self.eval_comments_in_block(&mut ast.statements);
        self.is_processing_comments = false;
        Ok(())
    }

    fn eval_comments_in_block(&mut self, block: &mut [LineComment<'a>]) {
        self.env.push();

        for line in block {
            // TODO: allow comments to actually modify the next statement
            let replacement = if let CommentBody::Stmt(ref mut stmt) = line.body {
                // We do not evaluate nested comments.
                // QQQ: how should we handle errors occurring at comment-time?
                self.buf_stack.push(String::new());

                let is_success = match self.eval_stmt(stmt) {
                    Signal::Error(e) => {
                        // TODO: optionally disable this
                        eprintln!("Failed to evaluate a comment: {:?}", e);
                        None
                    }
                    _ => Some(()),
                };

                let code = self.arena.alloc(self.buf_stack.pop().unwrap());
                is_success.and_then(move |_| {
                    crate::parser::dank::file(code)
                        .map(|ast| Stmt {
                            span: 0..0,
                            kind: StmtKind::UnscopedBlock(ast.statements),
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

        self.env.pop();
    }

    fn eval_comments_in_stmt(&mut self, stmt: &mut Stmt<'a>) {
        match &mut stmt.kind {
            StmtKind::LetDecl(_, init) => init
                .as_mut()
                .map(|e| self.eval_comments_in_expr(e))
                .unwrap_or(()),
            StmtKind::FuncDecl(f) => {
                self.eval_comments_in_block(&mut f.body);
            }
            StmtKind::ExprStmt(expr) => self.eval_comments_in_expr(expr),
            StmtKind::Print(args) => args.iter_mut().for_each(|e| self.eval_comments_in_expr(e)),
            StmtKind::Block(b) => self.eval_comments_in_block(b),
            StmtKind::While(_, stmt) => self.eval_comments_in_stmt(stmt),
            StmtKind::UnscopedBlock(_) => unreachable!("Cannot appear while processing comments"),
            StmtKind::Return(v) => v
                .as_mut()
                .map(|v| self.eval_comments_in_expr(v))
                .unwrap_or(()),
            StmtKind::Break => (),
            StmtKind::Continue => (),
            StmtKind::Assignment(_, value) => self.eval_comments_in_expr(value),
        }
    }

    fn eval_comments_in_expr(&mut self, expr: &mut Expr<'a>) {
        match &mut expr.kind {
            ExprKind::ObjectLiteral(pairs) => pairs
                .iter_mut()
                .for_each(|(_, e)| self.eval_comments_in_expr(e)),
            ExprKind::LambdaLiteral(f) => {
                self.eval_comments_in_block(&mut f.body);
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
            ExprKind::Property(_, _) => (),
        }
    }

    fn eval_ast(&mut self, ast: &Ast<'a>) -> Result<Value<'a>, EvalError<'a>> {
        match self.eval_block(&ast.statements) {
            Signal::Error(e) => Err(e),
            rest => Ok(rest.extract_unwrap()),
        }
    }

    fn eval_block(&mut self, block: &[LineComment<'a>]) -> Signal<'a> {
        self.env.push();
        let result = self.eval_unscoped_block(block);
        self.env.pop();
        result
    }

    fn eval_unscoped_block(&mut self, block: &[LineComment<'a>]) -> Signal<'a> {
        for stmt in block.iter().filter_map(|line| line.stmt.as_ref()) {
            self.eval_stmt(stmt)?;
        }
        Value::Null.into()
    }

    fn eval_expr(&mut self, expr: &Expr<'a>) -> Signal<'a> {
        match &expr.kind {
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
                        bin_op!(__internal $self, left $op right)
                    }};
                    (__internal $self:ident, $left:ident + $right:ident) => {{
                        match ($left, $right) {
                            (Value::Num(l), Value::Num(r)) => Signal::Value(Value::Num(l + r)),
                            (Value::Str(l), Value::Str(r)) => Signal::Value(Value::Str(format!("{}{}", l, r).into())),
                            (l, r) => Signal::Error($self.bin_op_type_error(&l, &r, "+")),
                        }
                    }};
                    (__internal $self:ident, $left:ident / $right:ident) => {{
                        match ($left, $right) {
                            (Value::Num(l), Value::Num(r)) if r != 0.0 => Signal::Value(Value::Num(l / r)),
                            (Value::Num(_), Value::Num(r)) if r == 0.0 => Signal::Error(EvalError::RuntimeError("Attempted to divide by zero".into())),
                            (l, r) => Signal::Error($self.bin_op_type_error(&l, &r, "/")),
                        }
                    }};
                    (__internal $self:ident, $left:ident $op:tt $right:ident) => {{
                        #[allow(clippy::float_cmp)]
                        match ($left, $right) {
                            (Value::Num(l), Value::Num(r)) => Signal::Value(Value::from(l $op r)),
                            (l, r) => Signal::Error($self.bin_op_type_error(&l, &r, stringify!($op))),
                        }
                    }};
                }
                match op {
                    BinOpKind::Add => bin_op!(self, l + r),
                    BinOpKind::Sub => bin_op!(self, l - r),
                    BinOpKind::Div => bin_op!(self, l * r),
                    BinOpKind::Mul => bin_op!(self, l / r),
                    BinOpKind::Eq => bin_op!(self, l == r),
                    BinOpKind::Ne => bin_op!(self, l != r),
                    BinOpKind::Lt => bin_op!(self, l < r),
                    BinOpKind::Le => bin_op!(self, l <= r),
                    BinOpKind::Gt => bin_op!(self, l > r),
                    BinOpKind::Ge => bin_op!(self, l >= r),
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
                        "Cannot apply the operator `-` to {}",
                        operand
                    ))
                    .into(),
                }
            }
            ExprKind::Property(name, obj) => {
                let obj = self.eval_expr(obj)?;
                self.get_prop(&obj, name.clone())
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
                let result = self.eval_block(&func.body);
                let _ = self.env.pop();
                result.map_return()
            }
            Value::NativeFn(func) => {
                if func.arity as usize != args.len() {
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
            Value::Obj(_) => match self.get_prop(callee, "__call__".into()).extract() {
                Some(method) => self.call(&method, args),
                None => EvalError::NotCallable(format!(
                    "Object {} is missing the __call__ property so it cannot be called",
                    callee
                ))
                .into(),
            },
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
            _ => EvalError::UndefinedProperty(name).into(),
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt<'a>) -> Signal<'a> {
        match &stmt.kind {
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
            StmtKind::Block(b) => return self.eval_block(b),
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
        if self.is_processing_comments {
            self.buf_stack.last_mut().unwrap().push_str(&s);
        } else {
            print!("{}", s)
        }
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
        }
    }

    fn bin_op_type_error(&self, l: &Value<'a>, r: &Value<'a>, op: &str) -> EvalError<'a> {
        EvalError::TypeError(format!(
            "Cannot apply the operator `{}` to {} and {}",
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
