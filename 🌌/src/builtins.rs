use crate::ast::Statement;
use crate::interpreter::Value;
use gc::{Gc, GcCell};
use qp_trie::{wrapper::BString, Trie};
use rand::Rng;

pub type BuiltinFunction = fn(Vec<Value>) -> Result<Value, String>;

fn create_builtin_statement(func: BuiltinFunction) -> Statement {
    Statement::CallBuiltin { function: func }
}

pub fn builtins() -> Trie<BString, Statement> {
    [
        ("pow", pow as BuiltinFunction),
        ("read line", read_line as BuiltinFunction),
        ("print", print as BuiltinFunction),
        ("println", println as BuiltinFunction),
        ("length of", len as BuiltinFunction),
        ("floating point cast", float as BuiltinFunction),
        ("integer cast", integer as BuiltinFunction),
        ("split string", split_string as BuiltinFunction),
        ("random integer", random_integer as BuiltinFunction),
        ("get char in string", get_char_in_string as BuiltinFunction),
    ]
    .iter()
    .map(|(name, func)| ((*name).into(), create_builtin_statement(*func)))
    .collect()
}

fn pow(args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        Err(format!(
            "builtin `pow` requires exactly 2 arguments, got {}",
            args.len()
        ))
    } else {
        args[0].pow(args[1].clone())
    }
}

fn read_line(_args: Vec<Value>) -> Result<Value, String> {
    let mut buf = String::new();
    std::io::stdin()
        .read_line(&mut buf)
        .map_err(|e| format!("in builtin `read line`: IO Error: {}", e))?;
    Ok(Value::String(buf))
}

fn print(args: Vec<Value>) -> Result<Value, String> {
    for value in args {
        print!("{}", value);
    }
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    Ok(Value::Null)
}

fn println(args: Vec<Value>) -> Result<Value, String> {
    for value in args {
        print!("{}", value);
    }
    println!();
    Ok(Value::Null)
}

fn len(arr: Vec<Value>) -> Result<Value, String> {
    match &arr.first() {
        Some(Value::Array(inner)) => Ok(Value::Integer(inner.borrow().len() as i64)),
        Some(Value::String(inner)) => Ok(Value::Integer(inner.len() as i64)),
        _ => Err("builtin `length of` requires an array or string type argument".to_owned()),
    }
}

fn float(args: Vec<Value>) -> Result<Value, String> {
    match &args.first() {
        Some(Value::Float(a)) => Ok(Value::Float(*a)),
        Some(Value::Integer(a)) => Ok(Value::Float(*a as f64)),
        Some(Value::String(a)) => Ok(Value::Float(a.trim().parse::<f64>().map_err(|_| {
            format!(
                "Given string \"{}\" cannot be parsed as a floating point number",
                a
            )
        })?)),
        Some(a) => Err(format!("{} cannot be cast to float", a.describe_type())),
        _ => Err(
            "builtin `floating point cast` requires an integer, float or string argument"
                .to_owned(),
        ),
    }
}

fn integer(args: Vec<Value>) -> Result<Value, String> {
    match &args.first() {
        Some(Value::Float(a)) => Ok(Value::Integer(*a as i64)),
        Some(Value::Integer(a)) => Ok(Value::Integer(*a as i64)),
        Some(Value::String(a)) => {
            Ok(Value::Integer(a.trim().parse::<i64>().map_err(|_| {
                format!("Given string \"{}\" cannot be parsed as a integer", a)
            })?))
        }
        Some(a) => Err(format!("{} cannot be cast to float", a.describe_type())),
        _ => Err("builtin `integer cast` requires an integer, float or string argument".to_owned()),
    }
}

fn split_string(args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!(
            "builtin `split string` requires exactly 2 arguments, got {}",
            args.len()
        ));
    }

    if let Value::String(s) = &args[0] {
        if let Value::String(p) = &args[1] {
            let elms = Gc::new(GcCell::new(
                s.split(p).map(|s| Value::String(s.to_string())).collect(),
            ));
            return Ok(Value::Array(elms));
        } else {
            return Err(
                "the second argument to builtin `split string` must be a string".to_string(),
            );
        }
    } else {
        return Err("the first argument to builtin `split string` must be a string".to_string());
    }
}

fn random_integer(_args: Vec<Value>) -> Result<Value, String> {
    Ok(Value::Integer(rand::thread_rng().gen()))
}

fn get_char_in_string(args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!(
            "builtin `get char in string` requires exactly 2 arguments, got {}",
            args.len()
        ));
    }

    if let Value::String(s) = &args[0] {
        if let Value::Integer(i) = args[1] {
            if i < 0 || i as usize >= s.chars().count() {
                return Err("out of bounds string index in `get char in string`".to_string());
            }

            let chr = s.chars().nth(i as usize).unwrap().to_string();
            return Ok(Value::String(chr));
        } else {
            return Err(
                "the second argument to builtin `get char in string` must be an integer"
                    .to_string(),
            );
        }
    } else {
        return Err(
            "the first argument to builtin `get char in string` must be a string".to_string(),
        );
    }
}
