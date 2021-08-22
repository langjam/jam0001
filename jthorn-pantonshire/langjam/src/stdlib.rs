use std::io::{self, Write};

use crate::value::Value;

macro_rules! std_fns {
    ($($p:pat => $e:expr),*) => {
        pub fn is_std_fn(name: &str) -> bool {
            match name {
                $($p => true),*,
                _ => false,
            }
        }

        pub fn call_std_fn(name: &str, args: &[Value]) -> Option<Value> {
            match name {
                $($p => Some($e(args))),*,
                _ => None,
            }
        }
    }
}

std_fns! {
    "print" => print
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
