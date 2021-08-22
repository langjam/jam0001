use std::io::{self, Write};

use crate::value::Value;

pub fn is_std_fn(name: &str) -> bool {
    match name {
        "print" => true,
        _ => false,
    }
}

pub fn call_std_fn(name: &str, args: &[Value]) -> Option<Value> {
    match name {
        "print" => Some(print(args)),
        _ => None,
    }
}

pub fn print(args: &[Value]) -> Value {
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    for arg in args {
        write!(&mut stdout, "{}", arg).expect("failed to write to stdout");
    }

    write!(&mut stdout, "\n").expect("failed to write to stdout");

    stdout.flush().expect("failed to flush stdout");

    Value::Void
}
