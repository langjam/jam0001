use toml::Value;

use std::env;
use std::fs;
use std::fmt;
use std::collections::HashMap;

struct Env {
    env: HashMap<String, Exp>,
}

impl Default for Env {
    fn default() -> Self {
        let mut default = Self { env: Default::default() };

        default.env.insert(String::from("+"), Exp::Fn(|args| {
            let answer: f64 = args.iter().map(|v| {
                match v {
                    Value::Float(f) => *f,
                    Value::Integer(i) => *i as f64,
                    _ => panic!("you can only add numbers, silly"),
                }
            }).sum();

            Value::Float(answer)
        }));

        default
    }
}

#[derive(Clone)]
enum Exp {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Datetime(toml::value::Datetime),
    Array(toml::value::Array),
    Table(toml::value::Table),
    Fn(fn(&[Value]) -> Value),
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Exp::String(s) => write!(f, "{}", s),
            Exp::Integer(i) => todo!(),
            Exp::Float(float) => write!(f, "{}", float),
            Exp::Boolean(b) => todo!(),
            Exp::Datetime(dt) => todo!(),
            Exp::Array(a) => todo!(),
            Exp::Table(t) => todo!(),
            Exp::Fn(f) => todo!(),
        }
    }
}

impl From<toml::Value> for Exp {
    fn from(toml: toml::Value) -> Self {
        match toml {
            Value::String(s) => Exp::String(s),
            Value::Integer(i) => Exp::Integer(i),
            Value::Float(i) => Exp::Float(i),
            Value::Boolean(b) => Exp::Boolean(b),
            Value::Datetime(dt) => Exp::Datetime(dt),
            Value::Array(a) => Exp::Array(a),
            Value::Table(t) => Exp::Table(t),
        }
    }
}

fn main() {

    let mut args = env::args();

    // throw away program name
    args.next();

    let path = args
        .next()
        .expect("Please pass a file path as the first argument");

    let toml = fs::read_to_string(path).expect("Could not read the file");
    let value = toml.parse::<Value>().unwrap();

    let program = Exp::from(value["main"].clone());

    let mut env = Env::default();

    let result = eval(&program, &mut env);

    println!("{}", result);
}

fn eval(exp: &Exp, env: &mut Env) -> Exp {
    match exp {
        Exp::String(_) => exp.clone(),
        Exp::Array(values) => {
            match &values[0] {
                Value::String(s) => {
                    let args = &values[1..];
                    match env.env.get(s) {
                        // if the string is a function, we need to treat it like one
                        Some(Exp::Fn(f)) => {
                            f(args).into()
                        }
                        Some(_) => panic!("stored a value that's not a function"),
                        // but if it's not, we want to just leave it as-is
                        None => {
                           values[0].clone().into()
                        }
                    }
                }
                rest => rest.clone().into()
            }
        }
        _ => todo!(),
    }
}
