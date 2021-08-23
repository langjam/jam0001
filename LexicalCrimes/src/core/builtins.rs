use super::{
    lexer::{Lexer, Analyser},
    tokens::Token,
    states::{ProgramState, Operation},
    messages::*,
};
use std::panic;
use std::process;

pub struct Function {
    pub(super) keyword: String,
    pub(super) value: String,
}

pub trait Builtin {
    fn execute(&self) -> Option<String>;
    fn print(&self);
    fn input(&self) -> String;
    fn for_loop(&self);
    fn loop_back();
    fn math_evaluator(expression: &str) -> f64;
}

impl Builtin for Function {
    // execute handles all function calls and optionally returns a value if there is
    fn execute(&self) -> Option<String> {
        let keyword = self.keyword.as_str();
        match keyword {
            // default functions
            "print" => {
                self.print();
                None
            },
            "loop" => {
                self.for_loop();
                None
            },

            // special functions
            "input()" => {
                Some(self.input())
            },

            // who dis?
            _ => {
                push_error(
                    format!("Function named `{}` does not exist.", self.keyword)
                );
                process::exit(1);
            },
        }
    }

    fn print(&self) {
        use std::io::{BufWriter, Write};
        use unescape::unescape;

        // get a locked buffered writer to stdout
        let stdout = std::io::stdout();
        let mut stdout = BufWriter::new(stdout.lock());
        
        // string formatting
        let mut chars: std::str::Chars;
        let fmt_out: &str;

        // format based on tokens
        match Lexer::tokenize(&Lexer, &self.value)[0] {
            // numbers represent as they are
            Token::Number => fmt_out = &self.value,
            // strings/objects need formatting (avoid pretty print)
            _ => {
                chars = self.value.trim().chars();
                chars.next();
                chars.next_back();
                fmt_out = chars.as_str()
            }
        }

        // escape sequence handling shouldn't panic
        let escaping = panic::catch_unwind(|| {
            unescape(fmt_out).unwrap()
        });

        let out = if escaping.is_ok() {
            unescape(fmt_out).unwrap()
        } else {
            push_error(
                format!(
                    "Panicked because of unsupported escape sequence on string: \"{}\"",
                    fmt_out
                )
            );
            process::exit(1)
        };

        // write formatted string to `stdout`
        let writeln = writeln!(
            stdout,
            "{}",
            out
        );
    
        match writeln {
            Ok(()) => (),
            Err(error) => panic!("Couldn't write to `stdout`: {:?}", error),
        };
    }

    fn math_evaluator(expression: &str) -> f64 {
        use fasteval::{ez_eval, EmptyNamespace};
        let mut ns = EmptyNamespace;

        let result = match ez_eval(expression, &mut ns) {
            Ok(result) => result,
            Err(error) => {
                push_error(
                    format!("Could not evaluate math expression \n\n\t{}\ndue to ↓ \n\n\t{:?}\n", expression, error)
                );
                process::exit(1)
            }
        };

        result
    }

    fn for_loop(&self) {
        // get iteration count
        let loop_count = &self.value;

        // supplied NaN check
        let value = match loop_count.parse::<i32>() {
            Ok(value) => value,
            Err(_) => {
                push_error(
                    format!("Loop requires an integer for iteration but received `{}`.", loop_count)
                );
                process::exit(1)
            }
        };

        // set the program state to loop with's count
        ProgramState::set_state(
            Token::Loop,
            Operation::Loop(value),
            0
        )
    }

    fn loop_back() {
        use super::memory::{MemoryLayout, Value, Manager};

        let mem_fetch = MemoryLayout::fetch("<CODEBODY>");

        if mem_fetch.is_some() {
            let code_slice = match MemoryLayout::fetch("<CODEBODY>").unwrap() {
                Value::String(code_slice) => code_slice,
                _ => unreachable!(),
            };
    
            let state = ProgramState::read_state();
    
            let iter_count = match state.operation {
                Operation::Loop(iter_count) => iter_count,
                _ => 0,
            };
    
            for _ in 0..(iter_count-1) {
                Lexer::analyse(
                    &Lexer, &code_slice
                );
            }
        }
    }

    fn input(&self) -> String {
        // input constructor
        let mut line = String::new();
        
        let message = &self.value;

        // print the user supplied input message as is
        let print_message = Function {
            keyword: "print".to_string(),
            value: message.to_string(),
        };
        Function::execute(&print_message);

        // this doesn't resolve but a panic here is less likely
        std::io::stdin().read_line(&mut line).unwrap();

        // return constructed string from `stdin`
        line
    }
}