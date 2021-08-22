use toml::Value;

use std::collections::HashMap;
use std::env;
use std::f64::consts::PI;
use std::fmt;
use std::fs;

#[derive(Debug)]
struct Env {
    env: HashMap<String, Exp>,
}

impl Default for Env {
    fn default() -> Self {
        let mut default = Self {
            env: Default::default(),
        };

        default.env.insert(
            String::from("+"),
            Exp::Fn(|args, env| {
                let answer: f64 = args
                    .iter()
                    .map(|v| match eval(&Exp::from(v.clone()), env) {
                        Exp::Float(f) => f,
                        Exp::Integer(i) => i as f64,
                        _ => panic!("you can only add numbers, silly"),
                    })
                    .sum();

                Value::Float(answer)
            }),
        );
        default
            .env
            .insert(String::from("pi"), Exp::Fn(|_, _| Value::Float(PI)));

        default.env.insert(
            String::from("*"),
            Exp::Fn(|args, env| {
                let answer: f64 = args
                    .iter()
                    .map(|v| match eval(&Exp::from(v.clone()), env) {
                        Exp::Float(f) => f,
                        Exp::Integer(i) => i as f64,
                        _ => panic!("you can only multiply numbers, silly"),
                    })
                    .product();

                Value::Float(answer)
            }),
        );

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
    Fn(fn(&[Value], &mut Env) -> Value),
}

impl fmt::Debug for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Exp::String(s) => write!(f, "{}", s),
            Exp::Integer(i) => write!(f, "{}", i),
            Exp::Float(float) => write!(f, "{}", float),
            Exp::Boolean(b) => write!(f, "{}", b),
            Exp::Datetime(dt) => write!(f, "{}", dt),
            Exp::Array(a) => {
                let out = a
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({})", out)
            }
            Exp::Table(_t) => write!(f, "[table]"),
            Exp::Fn(_func) => write!(f, "[func]"),
        }
    }
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
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

fn exp_to_value(exp: Exp, env: &mut Env) -> toml::Value {
    match exp {
        Exp::String(s) => Value::String(s),
        Exp::Integer(i) => Value::Integer(i),
        Exp::Float(f) => Value::Float(f),
        Exp::Boolean(b) => Value::Boolean(b),
        Exp::Datetime(dt) => Value::Datetime(dt),
        Exp::Array(a) => Value::Array(a),
        Exp::Table(t) => Value::Table(t),
        Exp::Fn(func) => func(&[], env),
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
    check_first_class(&toml);
    let value = toml.parse::<Value>().unwrap();

    let program = Exp::from(
        value
            .get("main")
            .expect("you have to have a table with the 'main' key")
            .clone(),
    );

    let mut env = Env::default();

    let result = eval(&program, &mut env);

    println!("{}", result);
}

fn check_first_class(toml: &str) {
    // do we start with a #?
    if toml.bytes().next().unwrap() != b'#' {
        panic!("Sorry, you must start your program with a comment.");
    }

    // we want to check out the next 19 characters
    let start = &toml[1..21];

    let mut found = false;
    for byte in start.bytes() {
        if byte == b'\n' {
            found = true;
            break;
        } 
    }
    if !found { panic!("You must only have 20 characters of a comment") }

    // ... and then the rest
    let rest = &toml[21..];

    for byte in rest.bytes() {
        if byte == b'#' {
            panic!("You may not have any other #s, sorry, those are only for first class");
        }
    }
}

fn eval(exp: &Exp, env: &mut Env) -> Exp {
    match exp {
        Exp::String(s) => match env.env.get(s) {
            Some(a) => a.clone(),
            None => Exp::String(s.clone()),
        },
        Exp::Array(values) => {
            let first = &values[0];
            let rest: Vec<Exp> = values[1..].iter().map(|v| Exp::from(v.clone())).collect();

            // check for built-ins
            if let Value::String(s) = first {
                match s.as_str() {
                    "if" => return eval_if(&rest, env),
                    "begin" => return eval_begin(&rest, env),
                    "define" => return eval_define(&rest, env),
                    _ => {}
                }
            }

            let first = eval(&Exp::from(first.clone()), env);

            if let Exp::Fn(f) = first {
                let args: Vec<Value> = rest
                    .iter()
                    .map(|v| exp_to_value(eval(v, env), env))
                    .collect();

                f(&args, env).into()
            } else {
                panic!("'{}' needs to be a function", first);
            }
        }
        Exp::Integer(_) => exp.clone(),
        Exp::Float(_) => exp.clone(),
        Exp::Boolean(_) => exp.clone(),
        Exp::Datetime(_) => exp.clone(),
        Exp::Table(_) => exp.clone(),
        Exp::Fn(_) => panic!("what the eff is this"),
    }
}

fn eval_if(args: &[Exp], env: &mut Env) -> Exp {
    let test = args.get(0).expect("if with no test? sus");
    match eval(test, env) {
        Exp::Boolean(b) => {
            let result = if b { &args[1] } else { &args[2] };

            eval(result, env)
        }
        _ => panic!("test has to be a bool"),
    }
}

fn eval_begin(args: &[Exp], env: &mut Env) -> Exp {
    let mut ret = None;

    for arg in args {
        ret = Some(eval(arg, env));
    }

    ret.expect("begin must have arguments")
}

fn eval_define(args: &[Exp], env: &mut Env) -> Exp {
    match &args[0] {
        Exp::String(k) => {
            let v = eval(&args[1], env);

            env.env.insert(k.clone(), v.clone());

            v
        }
        _ => panic!("define must take a string"),
    }
}
