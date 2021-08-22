use std::collections::HashMap;
use std::iter::Peekable;

use crate::model::Keyword;
use crate::model::Op;
use crate::model::Term;

pub fn describe(terms: Vec<Box<Term>>) -> (VM, Vec<Box<Term>>) {
    let mut iter = terms.iter();
    let mut heap = HashMap::new();
    let mut instr = vec![];
    while let Some(t) = iter.next() {
        match &**t {
            Term::Key(k) => match k {
                Keyword::Function => {
                    desc_func(&mut iter, &mut heap);
                }
                _ => instr.push(t.clone()),
            },
            _ => instr.push(t.clone()),
        }
    }

    //Split up the 'VM' and the instructions to avoid mutable borrowing headaches
    (
        VM {
            heap,
            vars: HashMap::new(),
        },
        instr,
    )
}

fn desc_func<'a>(
    terms: &mut std::slice::Iter<'a, Box<Term>>,
    heap: &mut HashMap<String, Box<Func>>,
) {
    let name = if let Term::Ident(s) = &**terms.next().expect("Expected function name") {
        s
    } else {
        panic!("Expected function name");
    };
    let mut args = vec![];
    let mut body: Vec<Box<Term>> = vec![];

    //not sure if cloning is needed here but it's late and I don't feel like fighting with the borrow checker
    while let Some(t) = terms.next() {
        match &**t {
            Term::Key(k) => match k {
                Keyword::End => break,
                Keyword::Returns => {
                    body.push(t.clone());
                    body.push(Box::new(
                        *terms
                            .next()
                            .expect("Expected expression following 'returns'")
                            .clone(),
                    ));
                    break;
                }
                Keyword::Takes(l) => {
                    l.iter().for_each(|v| {
                        args.push(v.clone());
                    });
                }
                _ => body.push(t.clone()),
            },

            _ => body.push(t.clone()),
        }
    }

    heap.insert(name.clone(), Box::new(Func { args, body }));
}

#[derive(Default, Debug)]
pub struct VM {
    // I don't really know what a heap is and at this point I'm afraid to ask
    heap: HashMap<String, Box<Func>>,
    vars: HashMap<String, Value>,
}

pub fn run(ctx: &mut VM, instr: Vec<Box<Term>>) -> Value {
    let mut iter = instr.iter().peekable();
    while let Some(n) = iter.peek() {
        match &***n {
            Term::Key(Keyword::Equals(s)) => {
                iter.next();
                let val = step(ctx, &mut iter);
                ctx.vars.insert(s.into(), val.0);
            }
            _ => {
                let v = step(ctx, &mut iter);
                if v.1 {
                    //only return here if something's actually returning
                    return v.0;
                }
            }
        }
    }

    Value::None
}

fn step<'a>(ctx: &mut VM, instr: &mut Peekable<std::slice::Iter<'a, Box<Term>>>) -> (Value, bool) {
    if let Some(t) = instr.next() {
        match &**t {
            Term::Key(k) => match k {
                Keyword::Prints => {
                    let next = instr.next().expect("Expected value to print");

                    let text = match &**next {
                        Term::Ident(n) => {
                            if let Some(val) = ctx.vars.get(n) {
                                format!("{}", val)
                            } else {
                                format!("nothing")
                            }
                        }
                        Term::Key(Keyword::Nothing) => {
                            format!("nothing")
                        }
                        _ => {
                            format!("{}", eval(ctx, next))
                        }
                    };
                    if let Some(w) = instr.peek() {
                        match &***w {
                            Term::Key(Keyword::With(nl)) => match &**nl {
                                Term::Newline => {
                                    println!("{}", text);
                                }
                                Term::String(s) => print!("{}{}", text, s),
                                _ => print!("{}", text),
                            },
                            _ => print!("{}", text),
                        }
                    } else {
                        print!("{}", text);
                    }
                }
                Keyword::Call => {
                    if let Some(f) = instr.next() {
                        if let Term::Ident(s) = &**f {
                            let func = ctx
                                .heap
                                .get(s)
                                .expect(&format!("No such function '{}'", s))
                                .clone();
                            if let Some(b) = instr.peek() {
                                if let Term::Key(Keyword::With(a)) = &***b {
                                    instr.next();
                                    return (run_func(ctx, &*func, Some(a.clone())), false);
                                }
                            } else {
                                return (run_func(ctx, &*func, None), false);
                            }
                        } else {
                            panic!("Expected identifier after 'call'");
                        }
                    }
                }
                Keyword::Returns => {
                    if let Some(f) = instr.peek() {
                        return (eval(ctx, &*f), true);
                    }
                }
                _ => {}
            },
            term => {
                return (eval(ctx, term), false);
            }
        }
    }

    (Value::None, false)
}

// Panic! at the Rust code
fn run_func(ctx: &mut VM, func: &Func, args: Option<Box<Term>>) -> Value {
    let mut func_ctx = VM::default();
    func_ctx.heap = ctx.heap.clone();
    if let Some(t) = args {
        match *t {
            Term::List(a) => {
                if a.len() != func.args.len() {
                    panic!(
                        "Function takes {} arguments, got {}",
                        func.args.len(),
                        a.len()
                    );
                }
                let mut i = 0;
                func.args.iter().for_each(|val| {
                    func_ctx.vars.insert(val.clone(), eval(ctx, &*a[i]));
                    i += 1;
                });
            }
            Term::Key(Keyword::Nothing) => {
                if func.args.len() == 1 {
                    func_ctx.vars.insert(func.args[0].clone(), Value::None);
                } else if func.args.len() > 1 {
                    panic!("Function takes {} arguments, got 1", func.args.len());
                }
            }
            _ => {
                if func.args.len() != 1 {
                    panic!("Function takes {} arguments, got 1", func.args.len());
                }

                func_ctx.vars.insert(func.args[0].clone(), eval(ctx, &*t));
            }
        }
    }

    run(&mut func_ctx, func.body.clone())
}

fn eval(ctx: &VM, term: &Term) -> Value {
    match term {
        Term::Ident(s) => {
            if let Some(val) = ctx.vars.get(s) {
                val.clone()
            } else {
                Value::Name(s.into())
            }
        }
        Term::Bool(b) => Value::Bool(*b),
        Term::Number(n) => Value::Num(*n),
        Term::String(s) => Value::String(s.clone()),
        Term::Operation(op) => match op {
            Op::Add(left, right) => {
                let l = eval(ctx, &**left);
                let r = eval(ctx, &**right);
                l + r
            }
            Op::Multiply(left, right) => {
                let l = eval(ctx, &**left);
                let r = eval(ctx, &**right);
                l * r
            }
            Op::Subtract(left, right) => {
                let l = eval(ctx, &**left);
                let r = eval(ctx, &**right);
                l - r
            }
            Op::Divide(left, right) => {
                let l = eval(ctx, &**left);
                let r = eval(ctx, &**right);
                l / r
            }
        },
        _ => Value::None,
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Bool(bool),
    Num(f32),
    Name(String),
    None,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Num(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Name(s) => write!(f, "{}", s),
            Self::None => write!(f, "nothing"),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Value;
    fn add(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Num(n) => match rhs {
                Self::Num(r) => {
                    Self::Num(n + r)
                }
                Self::String(r) => {
                    Self::String(format!("{}{}", n, r))
                }
                _ => panic!("Can't add types {:?} and {:?}", self, rhs),
            },
            Self::String(s) => match rhs {
                Self::Num(r) => {
                    Self::String(format!("{}{}", s, r))
                }
                Self::String(r) => {
                    Self::String(format!("{}{}", s, r))
                }
                _ => panic!("Can't add types {:?} and {:?}", self, rhs),
            },
            _ => {
                panic!("Can't add types {:?} and {:?}", self, rhs);
            }
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Value;
    fn sub(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Num(n) => match rhs {
                Self::Num(r) => {
                    Self::Num(n - r)
                }
                _ => panic!("Can't subtract types {:?} and {:?}", self, rhs),
            },
            _ => {
                panic!("Can't add types {:?} and {:?}", self, rhs);
            }
        }
    }
}
impl std::ops::Mul for Value {
    type Output = Value;
    fn mul(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Num(n) => match rhs {
                Self::Num(r) => {
                    Self::Num(n * r)
                }
                _ => panic!("Can't add types {:?} and {:?}", self, rhs),
            },
            _ => {
                panic!("Can't add types {:?} and {:?}", self, rhs);
            }
        }
    }
}
impl std::ops::Div for Value {
    type Output = Value;
    fn div(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Num(n) => match rhs {
                Self::Num(r) => {
                    Self::Num(n / r)
                }
                _ => panic!("Can't add types {:?} and {:?}", self, rhs),
            },
            _ => {
                panic!("Can't add types {:?} and {:?}", self, rhs);
            }
        }
    }
}
#[derive(Debug, Clone)]
struct Func {
    args: Vec<String>,
    body: Vec<Box<Term>>,
}
