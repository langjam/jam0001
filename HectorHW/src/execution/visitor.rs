use crate::parsing::ast::{Program, StmtComment, Stmt, StmtBody, Expr};
use std::collections::{HashMap, HashSet};
use crate::parsing::token::Token;

use super::objects::Object;
use std::fmt::Write;
use crate::execution::predef::define_natives;


pub(super) struct State{
    pub(super) vars: HashMap<String, Object>,
    previuos_cond_true:bool,
    fired_at_least_once:bool,
    onces: HashSet<usize>,
}

impl State {
    fn new() -> Self {
        let mut s = State { vars: HashMap::new() ,
            previuos_cond_true:false,
            fired_at_least_once:false,
            onces: HashSet::new() };
        define_natives(&mut s);
        s
    }
}

pub fn run_program(p: &Vec<Stmt>) -> Result<(), String> {
    let mut state = State::new();

    run_sequence(p, &mut state)?;

    Ok(())
}

fn run_sequence(seq:&Vec<Stmt>, state: &mut State) -> Result<(), String> {

    let mut ip = 0;

    loop {
        ip = visit_stmt(seq, ip, state)?;
        if ip>=seq.len() {break;}
    }

    Ok(())
}

fn visit_stmt(seq:&Vec<Stmt>, ip:usize, state:&mut State) -> Result<usize, String> {

    let mut skip = false;

    for x in &seq[ip].1 {
        match x{
            StmtComment::OnlyWhen(cond) => {
                let res = visit_expr(&cond, state)?;
                //dbg!(cond);
                //dbg!("only when res", res);

                if res.is_true() {
                    state.previuos_cond_true = true;
                    state.fired_at_least_once = true;
                } else {
                    state.previuos_cond_true = false;
                    skip=true;
                }
            }
            StmtComment::Other(_) => {}
            StmtComment::Never => { skip=true; }
            StmtComment::OtherwiseWhen(cond) => {
                if state.fired_at_least_once {
                    skip=true;
                    state.previuos_cond_true=false;
                    continue}
                let res = visit_expr(&cond, state)?;
                //dbg!(cond);
                //dbg!("only when res", res);

                if res.is_true() {
                    state.previuos_cond_true = true;
                    state.fired_at_least_once = true;
                } else {
                    state.previuos_cond_true = false;
                    skip=true;
                }
            }
            StmtComment::Assert(cond) => {
                let res = visit_expr(&cond, state)?;
                if !res.is_true() {
                    Err(format!("assert failed at {}", cond.get_line()))?
                }
            }

            StmtComment::Otherwise => {if state.fired_at_least_once {
                skip=true;
                state.previuos_cond_true = false;
            }else{
                state.previuos_cond_true = true;
            }}
            StmtComment::Conditionally => {
                state.previuos_cond_true = false;
                state.fired_at_least_once = false;
            }
            StmtComment::And => {
                if !state.previuos_cond_true {
                    skip=true;
                }
            }
            StmtComment::Once(idx) => {
                if state.onces.insert(*idx) {
                    state.previuos_cond_true = true;
                    state.fired_at_least_once = true;
                }else{
                    state.fired_at_least_once = false;
                    state.previuos_cond_true = false;
                    skip = true;
                }
            }
        }

    }

    if skip {return Ok(ip+1)}



    match &seq[ip].0 {
        StmtBody::Assign(target, expr) => {

            let res = visit_expr(expr, state)?;

            state.vars.insert(target.unwrap_string().unwrap(), res.clone());
            state.vars.insert("$last".to_string(), res);
        }
        StmtBody::Print(_, expr) => {
            let res = visit_expr(expr, state)?;
            println!("{}", res);
            state.vars.insert("$last".to_string(), res);

        }
        StmtBody::Goto(_, label) => {
            let idx = find_target(seq, label)
                .ok_or(format!("cound not find goto target {}", label))?;
            let ip = idx;
            return Ok(ip);
        }
        StmtBody::Nop(_) => {}
        StmtBody::ExprStmt(e) => {
            let r = visit_expr(e, state)?;
            state.vars.insert("$last".to_string(), r);
        }
    }

    Ok(ip+1)
}

fn find_target(p: &Program, s:&str) -> Option<usize> {
    for (idx,item) in p.iter().enumerate() {
        if item.1.is_empty() {continue};

        for x in &item.1 {
            match x {
                StmtComment::OnlyWhen(_) => {}
                StmtComment::Other(text) => {
                    if text == s {
                        return Some(idx)
                    }
                }
                StmtComment::Never => {}
                StmtComment::Otherwise => {}
                StmtComment::Conditionally => {}
                StmtComment::OtherwiseWhen(_) => {}
                StmtComment::And => {}
                StmtComment::Once(_) => {}
                StmtComment::Assert(_) => {}
            }
        }
    }

    None
}

fn visit_expr(expr: &Expr, state: &mut State) -> Result<Object, String>{
    match expr {
        Expr::Variable(v) => {
            state.vars.get(&*v.unwrap_string().unwrap())
                .ok_or(format!("undefined variable {:?}", v))
                .map(|o| {o.clone()})
        }
        Expr::Number(n) => {Ok(
            Object::Num(n.unwrap_number().unwrap())
        )}
        Expr::BinaryExpression(op, left, right) => {
            let left = visit_expr(left, state)?;
            //right may not be evaluated
            macro_rules! binary_numeric {
                ($op:tt, $left:expr, $right:expr, $disp_name:expr) => {
                    match ($left.unwrap_num(), $right.unwrap_num()){
                        (Some(n1), Some(n2)) => {
                            let res = n1 $op n2;
                            let res = Ok(Object::Num(res));
                            res
                        },
                        _ => {Err(format!("can only use numbers as {} operands", $disp_name))}
                    }
                }
            }

            macro_rules! binary_logic {
                ($op:tt, $left:expr, $right:expr, $disp_name:expr) => {
                    match ($left.unwrap_num(), $right.unwrap_num()){
                        (Some(n1), Some(n2)) => {
                            let res = if n1 $op n2 {1} else {0};
                            let res: Result<Object, String> = Ok(Object::Num(res));
                            res
                        },
                        _ => {Err(format!("can only use numbers as {} operands", $disp_name))}
                    }
                }
            }


            let res = match op {
                Token::Plus(_) => {
                    match (left, visit_expr(right, state)?) {
                        (Object::String(mut s), b) => {
                            write!(s, "{}", b).unwrap();
                            Object::String(s)
                        },
                        (b, Object::String(s)) => {
                            let mut pref = b.to_string();
                            write!(pref, "{}", s).unwrap();
                            Object::String(pref)
                        },
                        (Object::Num(n1), Object::Num(n2)) => {
                            Object::Num(n1+n2)
                        }
                        (Object::Function(_body1, _arity1), Object::Function(_body2, _arity2)) => {
                            Err(format!("operations on functions are not supported yet"))?
                        }

                            (a,b) => {
                                Err(format!("unsupported operands {} and {} for +", a, b))?
                            }
                    }
                }
                Token::Minus(_) => {binary_numeric!(-, left, visit_expr(right, state)?, "subtraction")?}
                Token::Star(_) => {
                    let left = left.unwrap_num();
                    if let Some(left) = left {
                        if left ==0 {
                            Object::Num(0)
                        }else{
                            let right = visit_expr(right, state)?;
                            match right.unwrap_num() {
                                Some(x) => Object::Num(left*x),
                                None => {
                                    Err("multiplication only support numbers")?
                                }
                            }

                        }
                    }else{
                        Err("multiplication only support numbers")?
                    }
                }
                Token::Slash(_) => {
                    let right = visit_expr(right, state)?;
                    require_num_types(&left, &right, "division".to_string())?;
                    let left = left.unwrap_num().unwrap();
                    let right = right.unwrap_num().unwrap();
                    if right==0 {Err("zero division")?};
                    Object::Num(left/right)
                }
                Token::Modulo(_) => {binary_numeric!(%, left, visit_expr(right, state)?, "modulo")?}
                Token::EqualsEquals(_) => {
                    //dbg!("==", left, right);
                    let left = left;
                    let right = visit_expr(right, state)?;
                    Object::Num(if left==right {1} else {0})
                }
                Token::GT(_) => {binary_logic!(>, left, visit_expr(right, state)?, "greater")?}
                Token::GE(_) => {binary_logic!(>=, left, visit_expr(right, state)?, "greater equal")?}
                Token::LT(_) => {binary_logic!(<, left, visit_expr(right, state)?, "less")?}
                Token::LE(_) => {binary_logic!(<=, left, visit_expr(right, state)?, "less equal")?}
                Token::NE(_) => {binary_logic!(!=, left, visit_expr(right, state)?, "non equal")?}
                x => panic!("invalid token {:?} as binary operator", x)
            };
            Ok(res)
        }

        Expr::String(s) => {
            Ok(Object::String(s.unwrap_string().unwrap().clone()))
        }
        Expr::UnaryExpression(op, target) => {
            let target = visit_expr(target, state)?;
            match op {
                Token::Meh(_) => {
                    if let Some(target) = target.unwrap_num() {
                        Ok(Object::Num(if target == 0 {1} else {0}))
                    }else{
                        Err("can only meh numbers")?
                    }
                }
                x => panic!("unsupported unary {:?}", x)
            }

        }
        Expr::Call(target, args_) => {
            let target = visit_expr(target, state)?;
            let mut args = vec![];
            for item in args_ {
                let res = visit_expr(item, state)?;
                args.push(res);
            }

            call_function(target, args, state)

        }
        Expr::FunctionDeclaration(_token, stmt_list, arg_list) => {
            Ok(Object::Function(stmt_list.clone(), arg_list.clone()))
        }
    }
}

fn require_num_types(ob1:&Object, ob2:&Object, operation:String) -> Result<(), String> {
    if ob1.unwrap_num().is_none() || ob2.unwrap_num().is_none() {
        Err(format!("operands for {} may only be numbers", operation))
    }else{
        Ok(())
    }
}

fn call_function(target:Object, args:Vec<Object>, _state: &mut State)
    -> Result<Object, String> {

    //println!("call on {} with {} args", target, args.len());

    let f = target.clone();

    match target {
        Object::Function(body, arg_list) => {
            let mut state = State::new();

            if args.len()!=arg_list.len() {
                Err(format!("wrong number of arguments: expected {} but got {}",
                    arg_list.len(), args.len()))?
            }

            for i in 0..arg_list.len() {
                let key = arg_list[i].unwrap_string().unwrap();
                let v = args[i].clone();

                state.vars.insert(key, v);

            }

            state.vars.insert("self".to_string(), f);

            run_sequence(&body, &mut state)?;
            let res = state.vars.get("$last").cloned().unwrap_or(Object::Num(0));
            return Ok(res);
        }

        Object::NativeFunction(f, arity) => {
            if args.len() != arity {
                Err(format!("wrong number of arguments: expected {} but got {}",
                            arity, args.len()))?
            }

            let res = f(args);
            Ok(res)
        }
        _ => Err("can only call functions")?
    }
}