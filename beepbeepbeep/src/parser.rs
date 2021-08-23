use std::collections::HashMap;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

pub(crate) type Fns = HashMap<String, usize>;

pub fn parse(input: &str) -> (Vec<Op>, Fns) {
    // a hack but it's fine, no need for sophistication
    let s = String::from("\n") + input + "\n\n";
    handle_function_delimiters(grammar::CommentsParser::new().parse(&s).unwrap())
}

enum State {
    No,
    Yes(usize)
}

fn handle_function_delimiters(ops: Vec<Op>) -> (Vec<Op>, Fns) {
    let mut out = vec![];
    let mut state = State::No;
    let mut fns = HashMap::new();
    for (i, op) in ops.into_iter().enumerate() {
        match op {
            Op::PlainFunction(n) => {
                if let State::Yes(_) = state {
                    panic!("no nested functions")
                }
                fns.insert(n, i);
                out.push(Op::JumpForward(0));
                state = State::Yes(i);
            },
            Op::FunctionWithArgs(n, _) => {
                if let State::Yes(_) = state {
                    panic!("no nested functions")
                }
                fns.insert(n, i);
                out.push(Op::JumpForward(0));
                state = State::Yes(i);
            },
            Op::Placeholder => {
                match state {
                    State::No => panic!("can't escape from a nonexistent scope"),
                    State::Yes(j) => out[j] = Op::JumpForward(i - j),
                }
                out.push(Op::Return);
                state = State::No;
            },
            _ => out.push(op)
        }
    }

    (out, fns)
}

type Name = String;

#[derive(Debug, Clone)]
pub enum Op {
    // Stack arithmetic
    Increment,
    Add,
    Mul,
    Pow,
    Sub,
    Div,
    Mod,
    // Named arithmetic
    IncrementOne(Name),
    AddOne(Name),
    MulOne(Name),
    PowOne(Name),
    SubOne(Name),
    DivOne(Name),
    ModOne(Name),
    AddTwo(Name, Name),
    MulTwo(Name, Name),
    PowTwo(Name, Name),
    SubTwo(Name, Name),
    DivTwo(Name, Name),
    ModTwo(Name, Name),
    // Control flow
    PlainFunction(Name),
    FunctionWithArgs(Name, usize),
    Placeholder,
    Return,
    Call(Name),
    CallWithArgs(Name, Vec<Name>),
    JumpForward(usize),
    SkipIfTrue,
    // Conditions & other stack stuff
    Equal,
    Nonzero,
    Dup,
    Push(Name),
    Pop(Name),
    PushNumber(u64),
    // Strings & IO
    PushString(String),
    Serialize,
    StringOut,
    StringIn,
    Exit,
}