use std::{collections::HashMap, io::{stdin, stdout, Write}, process::exit};

use crate::parser::{Op, Fns};

macro_rules! pop {
    ($stack:expr) => {
        $stack.pop().expect("Pop from empty stack")
    };
}

macro_rules! name {
    ($names:expr, $scope:expr, $name:expr) => {
        // don't worry about the clones
        // it's fiiiiiiiiiiiiiiiiiiiiiiiiiiiine 
        $names.get_mut(&($scope.clone(), $name.clone())).expect("Name doesn't exist in the current scope")
    };
}

macro_rules! set {
    ($names:expr, $scope:expr, $name:expr, $value:expr) => {
        $names.insert(($scope.clone(), $name.clone()), $value)
    };
}

// i have no consideration for cleanliness.
// bow down before my code, mortals
// i am living evidence that the rust evangelism strike force is doomed
pub fn run((ops, fns): (Vec<Op>, Fns)) {
    let mut stack: Vec<u64> = vec![];
    let mut pc: usize = 0;
    let mut cond: bool = false;
    let mut names: HashMap<(String, String), u64> = HashMap::new();
    let mut ret_stack: Vec<usize> = vec![];
    let mut scope = String::new();
    let mut out = stdout();
    let r#in = stdin(); // i am the queen of code quality
    while pc < ops.len() {
        let op = &ops[pc];
        pc += 1;
        match op {
            Op::Increment => {
                let x = pop!(stack);
                stack.push(x + 1)
            },
            Op::Add => {
                let x = pop!(stack);
                let y = pop!(stack);
                stack.push(x + y)
            },
            Op::Mul => {
                let x = pop!(stack);
                let y = pop!(stack);
                stack.push(x * y)
            },
            Op::Pow => {
                let x = pop!(stack);
                let y = pop!(stack);
                stack.push(x.pow(y as u32));
            },
            Op::Sub => {
                let x = pop!(stack);
                let y = pop!(stack);
                stack.push(x - y)
            },
            Op::Div => {
                let x = pop!(stack);
                let y = pop!(stack);
                stack.push(x / y)
            },
            Op::Mod => {
                let x = pop!(stack);
                let y = pop!(stack);
                stack.push(x % y)
            },
            Op::IncrementOne(n) => {
                let x = *name!(names, scope, n);
                set!(names, scope, n, x + 1);
            },
            Op::AddOne(n) => {
                let x = pop!(stack);
                let y = *name!(names, scope, n);
                set!(names, scope, n, x + y);
            },
            Op::MulOne(n) => {
                let x = pop!(stack);
                let y = *name!(names, scope, n);
                set!(names, scope, n, x * y);
            },
            Op::PowOne(n) => {
                let x = pop!(stack);
                let y = *name!(names, scope, n);
                set!(names, scope, n, x.pow(y as u32));
            },
            Op::SubOne(n) => {
                let x = pop!(stack);
                let y = *name!(names, scope, n);
                set!(names, scope, n, x - y);
            },
            Op::DivOne(n) => {
                let x = pop!(stack);
                let y = *name!(names, scope, n);
                set!(names, scope, n, x / y);
            },
            Op::ModOne(n) => {
                let x = pop!(stack);
                let y = *name!(names, scope, n);
                set!(names, scope, n, x % y);
            },
            Op::AddTwo(n, m) => {
                let x = *name!(names, scope, n);
                let y = *name!(names, scope, m);
                stack.push(x + y);
            },
            Op::MulTwo(n, m) => {
                let x = *name!(names, scope, n);
                let y = *name!(names, scope, m);
                stack.push(x * y);
            },
            Op::PowTwo(n, m) => {
                let x = *name!(names, scope, n);
                let y = *name!(names, scope, m);
                stack.push(x.pow(y as u32));
            },
            Op::SubTwo(n, m) => {
                let x = *name!(names, scope, n);
                let y = *name!(names, scope, m);
                stack.push(x - y);
            },
            Op::DivTwo(n, m) => {
                let x = *name!(names, scope, n);
                let y = *name!(names, scope, m);
                stack.push(x / y);
            },
            Op::ModTwo(n, m) => {
                let x = *name!(names, scope, n);
                let y = *name!(names, scope, m);
                stack.push(x % y);
            },
            Op::PlainFunction(_) |
            Op::FunctionWithArgs(_, _) | // if it works it works
            Op::Placeholder => unreachable!("This should never happen"),
            Op::Return => {
                let ret = ret_stack.pop().expect("Popped from an empty return stack");
                scope = String::new();
                pc = ret;
            },
            Op::Call(n) => {
                ret_stack.push(pc);
                let idx = *fns.get(n).expect("function not defined");
                scope = n.clone();
                pc = idx + 1;
            },
            Op::CallWithArgs(n, ms) => {
                for m in ms.iter().rev() {
                    let x = *name!(names, scope, m);
                    stack.push(x);
                }
                ret_stack.push(pc);
                let idx = *fns.get(n).expect("function not defined");
                scope = n.clone();
                pc = idx + 1;
            },
            Op::JumpForward(x) => {
                pc += *x;
            },
            Op::SkipIfTrue => {
                if cond {
                    pc += 1;
                }
            },
            Op::Equal => {
                let x = pop!(stack);
                let y = pop!(stack);
                cond = x == y;
            },
            Op::Nonzero => {
                let x = pop!(stack);
                cond = x != 0;
            },
            Op::Dup => {
                let x = pop!(stack);
                stack.push(x);
                stack.push(x);
            },
            Op::Push(n) => {
                stack.push(*name!(names, scope, n));
            },
            Op::Pop(n) => {
                set!(names, scope, n, pop!(stack));
            },
            Op::PushNumber(x) => stack.push(*x),
            Op::PushString(s) => {
                let length = s.len() as u64;
                for c in s.chars().rev() {
                    stack.push(c as u64);
                }
                stack.push(length);
            },
            Op::Serialize => {
                let x = pop!(stack);
                let s = x.to_string();
                let length = s.len() as u64;
                for c in s.chars().rev() {
                    stack.push(c as u64);
                }
                stack.push(length);
            },
            Op::StringOut => {
                let length = pop!(stack);
                for _ in 0..length {
                    let mut buf = [0; 4];
                    let c = char::from_u32(pop!(stack) as u32).expect("Tried to convert a bad number to a character");
                    c.encode_utf8(&mut buf);
                    out.write(&buf).expect("couldn't write to stdout");
                }
                out.flush().expect("couldn't flush stdout");
            },
            Op::StringIn => {
                let mut s = String::new();
                let length = r#in.read_line(&mut s).expect("couldn't read from stdin");
                for c in s.chars().rev() {
                    stack.push(c as u64);
                }
                stack.push(length as u64);
            },
            Op::Exit => exit(0),
        }
    }
}