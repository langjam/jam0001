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
            ExprKind::Literal(_) => (),
            ExprKind::Variable(_) => (),
            ExprKind::Binary(l, _, r) => {
                self.eval_comments_in_expr(l);
                self.eval_comments_in_expr(r);
            }
            ExprKind::Unary(_, operand) => {
                self.eval_comments_in_expr(operand);
            }
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
            StmtKind::Block(b) => return self.eval_block(b),
            _ => unimplemented!(),
        }
        Ok(Value::Null)
    }
}

#[cfg(test)]
mod tests {}
