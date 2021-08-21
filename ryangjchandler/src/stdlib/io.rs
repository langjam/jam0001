use crate::interpreter::Interpreter;
use crate::value::Value;

pub fn println(_: &mut Interpreter, args: Vec<Value>) -> Value {
    for arg in args {
        println!("{}", arg);
    }

    Value::Null
}