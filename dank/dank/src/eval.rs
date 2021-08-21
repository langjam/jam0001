use std::borrow::Cow;

use crate::ast::*;
use crate::data::Ptr;
use crate::{data::Value, env::Env};

#[derive(Debug, Default)]
pub struct Evaluator<'a> {
    env: Env<Cow<'a, str>, Value<'a>>,
}

#[derive(Debug)]
pub enum EvalError {}

// TODO: return/break/continue signals
impl<'a> Evaluator<'a> {
    pub fn with_env(env: Env<Cow<'a, str>, Value<'a>>) -> Self {
        Self { env }
    }

    pub fn eval(&mut self, ast: &Ast<'a>) -> Result<(), EvalError> {
        Ok(())
    }

    fn eval_comments(&mut self, ast: &Ast<'a>) -> Result<(), EvalError> {
        Ok(())
    }

    fn eval_ast(&mut self, ast: &Ast<'a>) -> Result<Value, EvalError> {
        Ok(Value::Null)
    }

    fn eval_block(&mut self, block: &[Stmt<'a>]) -> Result<Value, EvalError> {
        for stmt in block {
            self.eval_stmt(stmt)?;
        }
        Ok(Value::Null)
    }

    fn eval_expr(&mut self, expr: &Expr<'a>) -> Result<Value<'a>, EvalError> {
        match &expr.kind {
            ExprKind::Literal(l) => Ok(l.clone()),
            ExprKind::Variable(v) => todo!(),
            ExprKind::ObjectLiteral(_) => todo!(),
            ExprKind::Binary(_, _, _) => todo!(),
            ExprKind::Unary(_, _) => todo!(),
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt<'a>) -> Result<Value, EvalError> {
        match &stmt.kind {
            StmtKind::Print(args) => {
                for (i, a) in args.iter().enumerate() {
                    let value = self.eval_expr(a)?;
                    print!("{:?}", value);
                    if i != args.len() - 1 {
                        print!(", ")
                    }
                }
                println!();
            }
            _ => unimplemented!(),
        }
        Ok(Value::Null)
    }
}
