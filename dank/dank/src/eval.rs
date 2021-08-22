use std::borrow::Cow;

use crate::ast::*;
use crate::data::{ObjPtr, Object};
use crate::{data::Value, env::Env};

#[derive(Debug, Default)]
pub struct Evaluator<'a> {
    env: Env<Cow<'a, str>, Value<'a>>,
}

#[derive(Debug)]
pub enum EvalError<'a> {
    ArityMismatch(String),
    NotCallable(String),
    UndefinedProperty(Cow<'a, str>),
    DuplicatedLiteralField(Cow<'a, str>),
    UndefinedVariable(Cow<'a, str>),
}

// TODO: return/break/continue signals
impl<'a> Evaluator<'a> {
    pub fn with_env(env: Env<Cow<'a, str>, Value<'a>>) -> Self {
        Self { env }
    }

    pub fn eval(&mut self, ast: &mut Ast<'a>) -> Result<(), EvalError<'a>> {
        self.eval_comments(ast)?;
        self.eval_ast(ast)?;
        Ok(())
    }

    fn eval_comments(&mut self, ast: &mut Ast<'a>) -> Result<(), EvalError<'a>> {
        // Comment-time environment
        self.env.push();

        self.eval_comments_in_block(&mut ast.statements);

        self.env.pop();

        Ok(())
    }

    fn eval_comments_in_block(&mut self, block: &mut [LineComment<'a>]) {
        for line in block {
            // TODO: allow comments to actually modify the next statement
            if let CommentBody::Stmt(ref mut stmt) = line.body {
                // We do not evaluate nested comments.
                // QQQ: how should we handle errors occurring at comment-time?
                match self.eval_stmt(stmt) {
                    Ok(_) => (),
                    Err(e) => {
                        // TODO: optionally disable this
                        eprintln!("Failed to evaluate a comment: {:?}", e);
                    }
                }
            }

            if let Some(stmt) = line.stmt.as_mut() {
                self.eval_comments_in_stmt(stmt);
            }
        }
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
        }
    }

    fn eval_comments_in_expr(&mut self, expr: &mut Expr<'a>) {
        match &mut expr.kind {
            ExprKind::ObjectLiteral(pairs) => pairs
                .iter_mut()
                .for_each(|(_, e)| self.eval_comments_in_expr(e)),
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

    fn eval_ast(&mut self, ast: &Ast<'a>) -> Result<Value, EvalError<'a>> {
        self.eval_block(&ast.statements)
    }

    fn eval_block(&mut self, block: &[LineComment<'a>]) -> Result<Value, EvalError<'a>> {
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
            ExprKind::Binary(_, _, _) => todo!(),
            ExprKind::Unary(_, _) => todo!(),
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
            Value::Obj(_) => todo!(),
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

    fn eval_stmt(&mut self, stmt: &Stmt<'a>) -> Result<Value, EvalError<'a>> {
        match &stmt.kind {
            StmtKind::Print(args) => {
                for (i, a) in args.iter().enumerate() {
                    let value = self.eval_expr(a)?;
                    print!("{}", value);
                    if i != args.len() - 1 {
                        print!(", ")
                    }
                }
                println!();
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
            _ => unimplemented!(),
        }
        Ok(Value::Null)
    }
}

#[cfg(test)]
mod tests {}
