use std::borrow::Cow;

use crate::ast::*;
use crate::data::Object;
use crate::{data::Value, env::Env};

#[derive(Debug)]
pub struct Evaluator<'a, 'b: 'a> {
    buf_stack: Vec<String>,
    env: Env<Cow<'a, str>, Value<'a>>,
    is_processing_comments: bool,
    arena: &'b bumpalo::Bump,
}

#[derive(Debug)]
pub enum EvalError<'a> {
    ArityMismatch(String),
    NotCallable(String),
    TypeError(String),
    UndefinedProperty(Cow<'a, str>),
    DuplicatedLiteralField(Cow<'a, str>),
    UndefinedVariable(Cow<'a, str>),
}

// TODO: return/break/continue signals
impl<'a, 'b> Evaluator<'a, 'b> {
    pub fn with_env(env: Env<Cow<'a, str>, Value<'a>>, arena: &'b bumpalo::Bump) -> Self {
        Self {
            env,
            buf_stack: Vec::new(),
            is_processing_comments: false,
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

                let is_success = self
                    .eval_stmt(stmt)
                    .map_err(|e| {
                        // TODO: optionally disable this
                        eprintln!("Failed to evaluate a comment: {:?}", e);
                    })
                    .map(|_| ())
                    .ok();

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
        self.eval_block(&ast.statements)
    }

    fn eval_block(&mut self, block: &[LineComment<'a>]) -> Result<Value<'a>, EvalError<'a>> {
        self.env.push();
        let result = self.eval_unscoped_block(block);
        self.env.pop();
        result
    }

    fn eval_unscoped_block(
        &mut self,
        block: &[LineComment<'a>],
    ) -> Result<Value<'a>, EvalError<'a>> {
        for stmt in block.iter().filter_map(|line| line.stmt.as_ref()) {
            self.eval_stmt(stmt)?;
        }
        Ok(Value::Null)
    }

    fn eval_expr(&mut self, expr: &Expr<'a>) -> Result<Value<'a>, EvalError<'a>> {
        match &expr.kind {
            ExprKind::Literal(l) => Ok(l.clone()),
            ExprKind::Variable(v) => self
                .env
                .get(v)
                .cloned()
                .ok_or_else(|| EvalError::UndefinedVariable(v.clone())),
            ExprKind::ObjectLiteral(lit) => {
                let mut object = Object::default();
                for (k, v) in lit {
                    let value = self.eval_expr(v)?;
                    if object.fields.contains_key(k) {
                        return Err(EvalError::DuplicatedLiteralField(k.clone()));
                    }
                    object.fields.insert(k.clone(), value);
                }
                Ok(Value::Obj(object.move_on_heap()))
            }
            ExprKind::LambdaLiteral(f) => {
                let fn_ref = crate::data::Ptr::new((&**f).clone());
                Ok(Value::Fn(fn_ref))
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
                            (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l + r)),
                            (Value::Str(l), Value::Str(r)) => Ok(Value::Str(format!("{}{}", l, r).into())),
                            (l, r) => Err($self.bin_op_type_error(&l, &r, "+")),
                        }
                    }};
                    (__internal $self:ident, $left:ident $op:tt $right:ident) => {{
                        match ($left, $right) {
                            (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l $op r)),
                            (l, r) => Err($self.bin_op_type_error(&l, &r, stringify!($op))),
                        }
                    }};
                }
                match op {
                    BinOpKind::Add => bin_op!(self, l + r),
                    BinOpKind::Sub => bin_op!(self, l - r),
                    BinOpKind::Div => bin_op!(self, l * r),
                    BinOpKind::Mul => bin_op!(self, l / r),
                    BinOpKind::Or => {
                        let l = self.eval_expr(l)?;
                        if self.is_truthy(&l) {
                            Ok(l)
                        } else {
                            self.eval_expr(r)
                        }
                    }
                    BinOpKind::And => {
                        let l = self.eval_expr(l)?;
                        let r = self.eval_expr(r)?;
                        if !self.is_truthy(&l) {
                            Ok(l)
                        } else {
                            Ok(r)
                        }
                    }
                }
            }
            ExprKind::Unary(op, operand) => {
                let operand = self.eval_expr(operand)?;
                match op {
                    UnOpKind::Not => Ok(Value::Bool(!self.is_truthy(&operand))),
                    UnOpKind::Neg if matches!(operand, Value::Num(_)) => {
                        let value = match operand {
                            Value::Num(n) => n,
                            _ => unreachable!(),
                        };
                        Ok(Value::Num(-value))
                    }
                    UnOpKind::Neg => Err(EvalError::TypeError(format!(
                        "Cannot apply the operator `-` to {}",
                        operand
                    ))),
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

    fn call(
        &mut self,
        callee: &Value<'a>,
        args: Vec<Value<'a>>,
    ) -> Result<Value<'a>, EvalError<'a>> {
        match callee {
            Value::Fn(func) => {
                if func.args.len() != args.len() {
                    return Err(EvalError::ArityMismatch(format!(
                        "{} expected {} arguments but was given {}",
                        callee,
                        func.args.len(),
                        args.len()
                    )));
                }

                self.env.push();
                for (arg, arg_value) in func.args.iter().zip(args.into_iter()) {
                    self.env.add(arg.clone(), arg_value);
                }
                self.eval_block(&func.body)?;
                let _ = self.env.pop();
                Ok(Value::Null)
            }
            Value::NativeFn(func) => {
                if func.arity as usize != args.len() {
                    return Err(EvalError::ArityMismatch(format!(
                        "{} expected {} arguments but was given {}",
                        callee,
                        func.arity,
                        args.len()
                    )));
                }

                (func.func)(args)
            }
            Value::Obj(_) => match self.get_prop(callee, "__call__".into()) {
                Ok(method) => self.call(&method, args),
                Err(_) => Err(EvalError::NotCallable(format!(
                    "Object {} is missing the __call__ property so it cannot be called",
                    callee
                ))),
            },
            Value::Num(_) | Value::Bool(_) | Value::Null | Value::Str(_) => Err(
                EvalError::NotCallable(format!("`{}` is not a callable object.", callee)),
            ),
        }
    }

    fn get_prop(
        &mut self,
        obj: &Value<'a>,
        name: Cow<'a, str>,
    ) -> Result<Value<'a>, EvalError<'a>> {
        match obj {
            Value::Obj(obj) => {
                let obj = obj.borrow();
                obj.fields
                    .get(&name)
                    .cloned()
                    .ok_or(EvalError::UndefinedProperty(name))
            }
            _ => Err(EvalError::UndefinedProperty(name)),
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt<'a>) -> Result<Value<'a>, EvalError<'a>> {
        match &stmt.kind {
            StmtKind::Print(args) => {
                for (i, a) in args.iter().enumerate() {
                    let value = self.eval_expr(a)?;
                    self.print(format!("{}", value));
                    if i != args.len() - 1 {
                        self.print(", ");
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
            StmtKind::Block(b) => return self.eval_block(b),
            StmtKind::UnscopedBlock(b) => return self.eval_unscoped_block(b),
            StmtKind::ExprStmt(e) => {
                self.eval_expr(e)?;
            }
            StmtKind::While(_, _) => todo!(),
        }
        Ok(Value::Null)
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
}

#[cfg(test)]
mod tests {}
