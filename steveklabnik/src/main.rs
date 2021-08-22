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
            Exp::Integer(i) => write!(f, "{}", i),
            Exp::Float(float) => write!(f, "{}", float),
            Exp::Boolean(b) => write!(f, "{}", b),
            Exp::Datetime(dt) => write!(f, "{}", dt),
            Exp::Array(a) => {
                write!(f, "(")?;

                // yeah this has an extra , at the end idk for now
                for v in a {
                    write!(f, "{}, ", v)?;
                }

                write!(f, ")")
            }
            Exp::Table(_t) => write!(f, "[table]"),
            Exp::Fn(_func) => write!(f, "[func]"),
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

    let program = Exp::from(value.get("main").expect("you have to have a table with the 'main' key").clone());

    let mut env = Env::default();

    let result = eval(&program, &mut env);

    println!("{}", result);
}

fn eval(exp: &Exp, env: &mut Env) -> Exp {
    match exp {
        Exp::String(_) => exp.clone(),
        Exp::Array(values) => {
            if let Value::String(s) = &values[0] {
                if let Some(Exp::Fn(f)) = env.env.get(s) {
                    // if the string is a function, we need to treat it like one
                    let args = &values[1..];

                    // we want to return out because we're done here
                    return f(args).into();
                }
            }

            panic!("can't find a function '{}'", values[0]);

        }
        Exp::Integer(_) => exp.clone(),
        Exp::Float(_) => exp.clone(),
        Exp::Boolean(_) => exp.clone(),
        Exp::Datetime(_) => exp.clone(),
        Exp::Table(_) => exp.clone(),
        Exp::Fn(_) => panic!("what the eff is this"),
    }
}
