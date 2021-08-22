use std::io::{self, Write};

use rand::Rng;

use crate::value::Value;
use crate::error::RuntimeErrorCause;

macro_rules! std_fns {
    ($($p:pat => $e:expr),*) => {
        pub fn is_std_fn(name: &str) -> bool {
            match name {
                $($p => true),*,
                _ => false,
            }
        }

        pub fn call_std_fn(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeErrorCause>> {
            match name {
                $($p => Some($e(args))),*,
                _ => None,
            }
        }
    }
}

std_fns! {
    "print" => print,
    "type" => type_name,
    "measure" => measure,
    "get" => get,
    "random" => random
    // "map" => map
}



pub fn print(args: &[Value]) -> Result<Value, RuntimeErrorCause> {
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    for arg in args {
        write!(&mut stdout, "{}", arg).expect("failed to write to stdout");
    }

    write!(&mut stdout, "\n").expect("failed to write to stdout");

    stdout.flush().expect("failed to flush stdout");

    Ok(Value::Void)
}

pub fn type_name(args: &[Value]) -> Result<Value, RuntimeErrorCause> {
    let arg = args.get(0)
        .ok_or_else(|| RuntimeErrorCause::MissingVariable("type function expects an argument".into()))?;

    Ok(Value::String(arg.type_name().to_owned()))
}

pub fn measure(args: &[Value]) -> Result<Value, RuntimeErrorCause> {
    let arg = args.get(0)
        .ok_or_else(|| RuntimeErrorCause::MissingVariable("measure function expects an argument".into()))?;

    match arg {
        Value::String(s) => Ok((s.chars().count() as i64).into()),
        Value::List(l) => Ok((l.len() as i64).into()),
        v => Err(RuntimeErrorCause::TypeError(format!("cannot measure the length of {}", v.type_name()))),
    }
}

pub fn get(args: &[Value]) -> Result<Value, RuntimeErrorCause> {
    let collection = args.get(0)
        .ok_or_else(|| RuntimeErrorCause::MissingVariable("get function expects two arguments".into()))?;

    let index = args.get(1)
        .ok_or_else(|| RuntimeErrorCause::MissingVariable("get function expects two arguments".into()))?;

    let index = match index {
        Value::Int(x) => *x,
        v => return Err(RuntimeErrorCause::TypeError(format!("cannot use {} as index", v.type_name()))),
    } as usize;

    match collection {
        Value::String(s) => s
            .chars()
            .nth(index)
            .ok_or_else(|| RuntimeErrorCause::OutOfBoundsError(format!("index {} is out of bounds", index)))
            .map(|c| Value::String(c.to_string())),

        Value::List(l) => l
            .get(index)
            .cloned()
            .ok_or_else(|| RuntimeErrorCause::OutOfBoundsError(format!("index {} is out of bounds", index))),

        v => return Err(RuntimeErrorCause::TypeError(format!("cannot get element from {}", v.type_name()))),
    }
}

pub fn random(_: &[Value]) -> Result<Value, RuntimeErrorCause> {
    Ok(rand::thread_rng().gen::<i64>().into())
}
