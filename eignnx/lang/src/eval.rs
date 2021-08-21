use core::panic;
use std::fmt::Display;

use crate::ast::{Op, Tm};

pub type Env = Vec<ValBinding>;

#[derive(Debug, Clone)]
pub enum Val {
    Void,
    Text(String),
    Nat(usize),
    Bool(bool),
    Closure(Env, String, Tm),
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Void => write!(f, "void"),
            Val::Text(txt) => write!(f, "{:?}", txt),
            Val::Nat(n) => write!(f, "{}", n),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Closure(_env, param, body) => write!(f, "[fn {} -> {}]", param, body),
        }
    }
}

impl Val {
    fn unwrap_text(&self) -> &str {
        if let Self::Text(txt) = self {
            txt
        } else {
            unreachable!()
        }
    }
    fn unwrap_nat(&self) -> usize {
        if let Self::Nat(x) = self {
            *x
        } else {
            unreachable!()
        }
    }
    fn unwrap_bool(&self) -> bool {
        if let Self::Bool(x) = self {
            *x
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValBinding {
    VarBind(String, Val),
}

pub struct Eval {
    env: Env,
}

impl Eval {
    pub fn new() -> Self {
        Self { env: vec![] }
    }

    fn lookup_var(&self, name: &str) -> Val {
        for binding in self.env.iter().rev() {
            match binding {
                ValBinding::VarBind(n, val) if n == name => return val.clone(),
                _ => continue,
            }
        }
        panic!("Unbound variable `{}`", name);
    }

    fn extend_env(&mut self, new_env: Env) {
        self.env.extend(new_env);
    }

    fn bind(&mut self, name: String, val: Val) {
        self.env.push(ValBinding::VarBind(name, val));
    }

    pub fn eval(&mut self, tm: &Tm) -> Val {
        match tm {
            Tm::Text(_loc, txt) => Val::Text(txt.clone()),

            Tm::Nat(_loc, n) => Val::Nat(*n),

            Tm::Bool(_loc, b) => Val::Bool(*b),

            Tm::Var(_loc, name) => self.lookup_var(&name),

            Tm::Lam(_loc, param, _ty, body) => {
                let creation_env = self.env.clone();
                let body = body.as_ref().clone();
                Val::Closure(creation_env, param.clone(), body)
            }

            Tm::App(_loc, func, arg) => {
                let func = self.eval(func);
                let arg = self.eval(arg);
                if let Val::Closure(env, param, body) = func {
                    let old_env = self.env.clone();
                    self.extend_env(env);
                    self.bind(param.clone(), arg.clone());
                    let result = self.eval(&body);
                    self.env = old_env;
                    result
                } else {
                    panic!("Expected a closure, got {:?}", func);
                }
            }

            Tm::Block(_loc, tms) => {
                if let Some((last, init)) = tms.split_last() {
                    let old_env = self.env.clone();
                    for tm in init {
                        self.eval(tm);
                    }
                    let result = self.eval(last);
                    self.env = old_env;
                    result
                } else {
                    Val::Void
                }
            }

            Tm::Def(_loc, name, body) => {
                let body = self.eval(body);
                self.bind(name.clone(), body);
                Val::Void
            }

            Tm::Op(loc, x_tm, op, y_tm) => {
                let x = self.eval(x_tm);
                let y = self.eval(y_tm);
                match op {
                    Op::SpaceConcat => {
                        let x = x.unwrap_text().to_owned();
                        let y = y.unwrap_text();
                        if x.ends_with(|ch| [' ', '\n', 't'].contains(&ch))
                            || y.starts_with(|ch| ['.', ',', ';'].contains(&ch))
                        {
                            Val::Text(x + y)
                        } else {
                            Val::Text(x + " " + y)
                        }
                    }
                    Op::Concat => Val::Text(x.unwrap_text().to_owned() + y.unwrap_text()),
                    Op::Add => Val::Nat(x.unwrap_nat() + y.unwrap_nat()),
                    Op::Sub => Val::Nat(x.unwrap_nat() - y.unwrap_nat()),
                    Op::Mul => Val::Nat(x.unwrap_nat() * y.unwrap_nat()),
                    Op::Div => Val::Nat(x.unwrap_nat() / y.unwrap_nat()),
                    Op::Apply => self.eval(&Tm::App(loc.clone(), x_tm.clone(), y_tm.clone())),
                    Op::And => Val::Bool(x.unwrap_bool() && y.unwrap_bool()),
                    Op::Or => Val::Bool(x.unwrap_bool() || y.unwrap_bool()),
                    Op::Lt => Val::Bool(x.unwrap_nat() < y.unwrap_nat()),
                    Op::Lte => Val::Bool(x.unwrap_nat() <= y.unwrap_nat()),
                    Op::Gt => Val::Bool(x.unwrap_nat() > y.unwrap_nat()),
                    Op::Gte => Val::Bool(x.unwrap_nat() >= y.unwrap_nat()),
                    Op::NatEq => Val::Bool(x.unwrap_nat() == y.unwrap_nat()),
                    Op::TextEq => Val::Bool(x.unwrap_text() == y.unwrap_text()),
                }
            }
        }
    }
}
