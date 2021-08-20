use std::fmt::Display;

use crate::ast::Tm;

pub type Env = Vec<ValBinding>;

#[derive(Debug, Clone)]
pub enum Val {
    Text(String),
    Closure(Env, String, Tm),
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Text(txt) => write!(f, "{:?}", txt),
            Val::Closure(_env, param, _body) => write!(f, "[fn {} -> ...]", param),
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
        }
    }
}
