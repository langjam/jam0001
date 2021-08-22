use std::collections::HashMap;

use crate::error::{RuntimeErrorCause, RuntimeResult};
use crate::value::Value;
use crate::parser::{Span, Program, BinaryOp, UnaryOp, Expr, ExprKind, Function, Stmt};
use crate::stdlib;

pub struct Interpreter {
    functions: HashMap<String, Function>,
}

#[derive(Copy, Clone, Debug)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Clone, Debug)]
pub struct Store(HashMap<String, (Value, Mutability)>);

impl Store {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get<'a, 'k>(&'a self, name: &'k str) -> Option<&'a (Value, Mutability)> {
        self.0.get(name)
    }

    pub fn store(&mut self, name: String, val: Value, mutability: Mutability) -> Result<(), RuntimeErrorCause> {
        if let Some((_, Mutability::Immutable)) = self.0.get(&name) {
            Err(RuntimeErrorCause::Immutable(format!("cannot mutate immutable variable {}", name)))
        } else {
            self.0.insert(name, (val, mutability));
            Ok(())
        }
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        let mut functions = HashMap::<String, Function>::new();

        for function in program.functions {
            functions.insert(function.name.clone(), function);
        }

        Self {
            functions,
        }
    }

    pub fn exec_main(&self) -> RuntimeResult<Value> {
        let main_function = self.functions
            .get("main")
            .expect("no main function found");

        self.exec_fn(main_function, Vec::new())
    }

    pub fn exec_fn(&self, function: &Function, args: Vec<Value>) -> RuntimeResult<Value> {
        let mut local = Store::new();
        
        for (param, arg) in function.params.iter().zip(args.into_iter()) {
            local.store(param.clone(), arg, Mutability::Mutable).ok();
        }

        for stmt in &function.body {
            if let Some(return_value) = self.eval_stmt(&mut local, stmt)? {
                return Ok(return_value);
            }
        }
        
        Ok(Value::Void)
    }

    fn eval_stmt(&self, local: &mut Store, stmt: &Stmt) -> RuntimeResult<Option<Value>> {
        match stmt {
            Stmt::Var(var_stmt) => {
                let val = self.eval_expr(&local, &var_stmt.value)?;
                local.store(var_stmt.variable.clone(), val, Mutability::Mutable)
                    .map_err(|err| err.error(var_stmt.value.span))?;
            },

            Stmt::Loop(loop_stmt) => {
                let iter = match self.eval_expr(local, &loop_stmt.loops)? {
                    Value::Int(loops) => loops,
                    v => return Err(RuntimeErrorCause::TypeError(format!("cannot use {} as number of loops", v.type_name()))
                        .error(loop_stmt.loops.span)),
                };

                for _ in 0..iter {
                    for stmt in &loop_stmt.stmts {
                        if let Some(ret) = self.eval_stmt(local, stmt)? {
                            return Ok(Some(ret));
                        }
                    }
                }
            },

            Stmt::Call(call_stmt) => {
                let call_args = call_stmt.args
                    .iter()
                    .map(|expr| self.eval_expr(&local, expr))
                    .collect::<RuntimeResult<Vec<_>>>()?;

                let function_name = local
                    .get(&call_stmt.function)
                    .map(|(val, _)| match val {
                        Value::Function(name) => Ok(name),
                        _ => Err(RuntimeErrorCause::TypeError(format!("cannot call {} which is not a function", call_stmt.function))
                            .error(call_stmt.span))
                    })
                    .transpose()?
                    .unwrap_or(&call_stmt.function);

                let call_out = if let Some(function) = self.functions.get(function_name) {
                    self.exec_fn(function, call_args)?
                } else if let Some(out) = stdlib::call_std_fn(function_name, &call_args) {
                    out.map_err(|err| err.error(call_stmt.span))?
                } else {
                    return Err(RuntimeErrorCause::MissingFunction(format!("no function called {} found", call_stmt.function))
                        .error(call_stmt.span));
                };

                let store = call_stmt.store.as_deref().unwrap_or("it");

                local.store(store.to_owned(), call_out, Mutability::Mutable)
                    .map_err(|err| err.error(call_stmt.span))?;
            },

            Stmt::Cond(cond_stmt) => {
                let cond = match self.eval_expr(&local, &cond_stmt.cond)? {
                    Value::Bool(a) => a,
                    v => return Err(RuntimeErrorCause::TypeError(format!("cannot use {} as condition for if statement", v.type_name()))
                        .error(cond_stmt.span)),
                };
                if cond {
                    for stmt in &cond_stmt.stmt1 {
                        if let Some(ret) = self.eval_stmt(local, stmt)? {
                            return Ok(Some(ret));
                        }
                    }
                } else {
                    for stmt in &cond_stmt.stmt2 {
                        if let Some(ret) = self.eval_stmt(local, stmt)? {
                            return Ok(Some(ret));
                        }
                    }
                }
            },

            Stmt::Cons(cons_stmt) => {
                let expr = self.eval_expr(&local, &cons_stmt.expr)?;
                local.store(cons_stmt.name.clone(), expr, Mutability::Immutable)
                    .map_err(|err| err.error(cons_stmt.span))?;
            },

            Stmt::Return(return_stmt) => {
                return Ok(Some(self.eval_expr(local, &return_stmt.expr)?))
            },
        } 

        Ok(None)
    }

    fn eval_expr(&self, local: &Store, expr: &Expr) -> RuntimeResult<Value> {
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
            
            ExprKind::Variable(variable) => local
                .get(variable)
                .map(|(val, _) | val.clone())
                .or_else(|| if self.functions.contains_key(variable) || stdlib::is_std_fn(variable) {
                    Some(Value::Function(variable.clone()))
                } else {
                    None
                })
                .ok_or_else(|| RuntimeErrorCause::MissingVariable(format!("variable {} is not defined", variable))
                    .error(expr.span)),

            ExprKind::List(exprs) => Ok(Value::List(exprs
                .iter()
                .map(|expr| self.eval_expr(local, expr))
                .collect::<RuntimeResult<Vec<_>>>()?)),
        }
    }

    fn visit_unary(&self, span: Span, op: &UnaryOp, val: Value) -> RuntimeResult<Value> {
        match op {
            UnaryOp::Not => match val {
                Value::Bool(a) => Ok(Value::Bool(!a)),
                v => Err(RuntimeErrorCause::TypeError(format!("boolean complement of a {} is not defined", v.type_name()))
                    .error(span)),
            },

            UnaryOp::Minus => match val {
                Value::Int(a) => Ok(Value::Int(-a)),
                v => Err(RuntimeErrorCause::TypeError(format!("negative of a {} is not defined", v.type_name()))
                    .error(span)),
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
                (Value::List(a), Value::List(b)) => Ok((a.len() < b.len()).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("cannot compare {} and {}", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Le => match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((!(a==false && b==true)).into()),
                (Value::Int(a), Value::Int(b)) => Ok((a <= b).into()),
                (Value::String(a), Value::String(b)) => Ok((a <= b).into()),
                (Value::List(a), Value::List(b)) => Ok((a.len() <= b.len()).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("cannot compare {} and {}", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Gt=> match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((a==true && b==false).into()),
                (Value::Int(a), Value::Int(b)) => Ok((a > b).into()),
                (Value::String(a), Value::String(b)) => Ok((a.len() > b.len()).into()),
                (Value::List(a), Value::List(b)) => Ok((a.len() > b.len()).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("cannot compare {} and {}", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Ge=> match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((!(a==true && b==false)).into()),
                (Value::Int(a), Value::Int(b)) => Ok((a >= b).into()),
                (Value::String(a), Value::String(b)) => Ok((a.len() >= b.len()).into()),
                (Value::List(a), Value::List(b)) => Ok((a.len() >= b.len()).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("cannot compare {} and {}", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::And => match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((a && b).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("boolean and of {} and {} is not defined", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Or => match (val1, val2) {
                (Value::Bool(a), Value::Bool(b)) => Ok((a || b).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("boolean or of {} and {} is not defined", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Add => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a + b).into()),
                (Value::List(a), Value::List(b)) => {
                    let mut vec = Vec::new();
                    for item in &a {
                        vec.push(item.clone());
                    };
                    for item in &b {
                        vec.push(item.clone());
                    };
                    Ok(vec.into())
                },
                (Value::String(a), Value::String(b)) => {
                    let mut buf = String::with_capacity(a.len() + b.len());
                    buf.push_str(&a);
                    buf.push_str(&b);
                    Ok(buf.into())
                },
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("addition of {} and {} is not defined", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Sub => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a - b).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("subtraction of {} and {} is not defined", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Mul => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a * b).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("multiplication of {} and {} is not defined", v1.type_name(), v2.type_name()))
                    .error(span)),
            },

            BinaryOp::Div => match (val1, val2) {
                (Value::Int(a), Value::Int(b)) => Ok((a / b).into()),
                (v1, v2) => Err(RuntimeErrorCause::TypeError(format!("division of {} and {} is not defined", v1.type_name(), v2.type_name()))
                    .error(span)),
            },
        }
    }
}
