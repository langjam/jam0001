use std::collections::HashMap;
use std::cell::RefCell;

use crate::error::{RuntimeErrorCause, RuntimeResult};
use crate::value::Value;
use crate::parser::{Span, Program, BinaryOp, UnaryOp, Expr, ExprKind, Function, Stmt};
use crate::stdlib;

pub struct Interpreter {
    functions: Store<Function>,
    global: RefCell<Store<Value>>,
}

pub type VariableStore = Store<Value>;

pub struct Store<T>(HashMap<String, T>);

impl<T> Store<T> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get<'a, 'k>(&'a self, name: &'k str) -> Option<&'a T> {
        self.0.get(name)
    }

    pub fn store(&mut self, name: String, val: T) {
        self.0.insert(name, val);
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        let mut functions = Store::new();

        for function in program.functions {
            functions.store(function.name.clone(), function);
        }

        Self {
            functions,
            global: RefCell::new(Store::new()),
        }
    }

    pub fn exec_main(&self) -> RuntimeResult<Value> {
        let main_function = self.functions
            .get("main")
            .expect("no main function found");

        self.exec_fn(main_function, &[])
    }

    //TODO: bind arguments to local variables
    pub fn exec_fn(&self, function: &Function, args: &[Value]) -> RuntimeResult<Value> {
        let mut local = VariableStore::new();
        
        for stmt in &function.body {
            match stmt {
                Stmt::Args(args_stmt) => todo!(),

                Stmt::Var(var_stmt) => {
                    let val = self.eval_expr(&local, &var_stmt.value)?;
                    local.store(var_stmt.variable.clone(), val);
                },

                Stmt::Loop(loop_stmt) => todo!(),

                Stmt::Call(call_stmt) => {
                    let call_args = call_stmt.args
                        .iter()
                        .map(|expr| self.eval_expr(&local, expr))
                        .collect::<RuntimeResult<Vec<_>>>()?;

                    let call_out = if let Some(function) = self.functions.get(&call_stmt.function) {
                        self.exec_fn(function, &call_args)?
                    } else if let Some(out) = stdlib::call_std_fn(&call_stmt.function, &call_args) {
                        out
                    } else {
                        return Err(RuntimeErrorCause::MissingFunction.error(call_stmt.span));
                    };

                    //TODO: do something with call_out
                },

                Stmt::Cond(cond_stmt) => todo!(),
            }
        }

        //TODO: returning values from functions
        Ok(Value::Void)
    }

    fn eval_expr(&self, local: &VariableStore, expr: &Expr) -> RuntimeResult<Value> {
        match &expr.kind {
            ExprKind::Value(value) => Ok(value.clone()),

            ExprKind::Unary(op, expr) => {
                let val = self.eval_expr(local, expr)?;
                self.visit_unary(expr.span, op, val)
            },

            ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.eval_expr(local, lhs)?;
                let rhs = self.eval_expr(local, rhs)?;
                self.visit_binary(expr.span, op, lhs, rhs)
            },
            
            ExprKind::Variable(variable ) => local
                .get(variable)
                .cloned()
                .ok_or_else(|| RuntimeErrorCause::MissingVariable.error(expr.span)),
        }
    }

    fn visit_unary(&self, span: Span, op: &UnaryOp, val: Value) -> RuntimeResult<Value> {
        match op {
            UnaryOp::Not => match val {
                Value::Bool(a) => Ok(Value::Bool(!a)),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            UnaryOp::Minus => match val {
                Value::Int(a) => Ok(Value::Int(-a)),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            }
        }
    }

    fn visit_binary(&self, span: Span, op: &BinaryOp, val1: Value, val2: Value) -> RuntimeResult<Value> {
        match op {
            BinaryOp::Eq => Ok((val1 == val2).into()),

            BinaryOp::Neq => Ok((val1 != val2).into()),

            BinaryOp::Lt => match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((a==false && b==true).into()),
                (Value::Int(a), Value::Int(b)) => Ok((a < b).into()),
                (Value::String(a), Value::String(b)) => Ok((a < b).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Le => match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((!(a==false && b==true)).into()),
                (Value::Int(a), Value::Int(b)) => Ok((a <= b).into()),
                (Value::String(a), Value::String(b)) => Ok((a <= b).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Gt=> match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((a==true && b==false).into()),
                (Value::Int(a), Value::Int(b)) => Ok((a > b).into()),
                (Value::String(a), Value::String(b)) => Ok((a.len() > b.len()).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Ge=> match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((!(a==true && b==false)).into()),
                (Value::Int(a), Value::Int(b)) => Ok((a >= b).into()),
                (Value::String(a), Value::String(b)) => Ok((a.len() >= b.len()).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::And => match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((a && b).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Or => match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((a || b).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Add => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a + b).into()),
                (Value::String(a), Value::String(b)) => {
                    let mut buf = String::with_capacity(a.len() + b.len());
                    buf.push_str(&a);
                    buf.push_str(&b);
                    Ok(buf.into())
                },
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Sub => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a - b).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Mul => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a * b).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },

            BinaryOp::Div => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a / b).into()),
                _ => Err(RuntimeErrorCause::TypeError.error(span)),
            },
        }
    }
}
