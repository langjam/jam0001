use core::panic;
use std::fmt::Display;

use crate::ast::Tm;

pub type Env = Vec<ValBinding>;

#[derive(Debug, Clone)]
pub enum Val {
    Void,
    Text(String),
    Closure(Env, String, Tm),
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Void => write!(f, "void"),
            Val::Text(txt) => write!(f, "{:?}", txt),
            Val::Closure(_env, param, body) => write!(f, "[fn {} -> {}]", param, body),
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
        }
    }
}
