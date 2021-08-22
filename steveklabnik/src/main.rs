use toml::Value;

use std::env;
use std::fs;
use std::collections::HashMap;

#[derive(Default)]
struct Env {
    env: HashMap<String, Value>,
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

    let program = &value["main"];

    let mut env = Env::default();

    let result = eval(program, &mut env);

    println!("{}", result);
}

fn eval(exp: &Value, env: &mut Env) -> Value {
    match exp {
        Value::String(_) => {
            exp.clone()
        }
        _ => todo!(),
    }
}
