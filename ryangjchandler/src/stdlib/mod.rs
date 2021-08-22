use crate::interpreter::Interpreter;

mod io;

pub fn register(interpreter: &mut Interpreter) {
    interpreter.register_stdlib_function("println", io::println);
}