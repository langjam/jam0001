use crate::parsing::ast::{Stmt, StmtComment, StmtBody, Expr};
use crate::parsing::token::{Token, Index};

pub mod token;
pub mod ast;

pub mod parser;

use parser::ProgramParser;
use std::cmp::max;

pub fn parse_str(input: &str) -> Result<Vec<Stmt>, String> {
    let (a, b) = token::split_comments(input);

    //let a =dbg!(a);

    let program_parser = ProgramParser::new();



    let tokens = token::tokenize(&a, 0)?.0;

    let program = program_parser.parse(tokens)
        .map_err(|e| {format!("{:?}", e)})?;

    let comments = b.iter()
        .map(|(k,v)|{
            (*k, parse_comment(v.as_str(), *k))
        }).collect::<Vec<(usize, StmtComment)>>();

    //assign comments to statements

    //1. get statement indices.

    let statements = program/*.into_iter().map(|stmt| {
        match &stmt {
            StmtBody::Assign(token, _) => {(token.get_line(), stmt)}
            StmtBody::Print(t, _) => {(t.get_line(), stmt)}
            StmtBody::Goto(t, _) => {(t.get_line(), stmt)}
            StmtBody::Nop(_) => {panic!("nop should not appear here")}
            StmtBody::ExprStmt(t) => {(t.get_line(), stmt)}
        }
    }).collect::<Vec<_>>()*/;

    //2. sort comments by line

    let mut comments = comments.into_iter().collect::<Vec<_>>();
    comments.sort_by(|a1, a2| {a1.0.cmp(&a2.0)});


    //3. bind comments to statements. Multiple comments are allowed for same statement.
    //if comment preceeds statement, it is considered bound to the statement

    /*for stmt in &statements{
        println!("{:?}", stmt);
    }


    for comment in &comments{
        println!("{} {:?}", comment.0, comment.1);
    }*/

    let mut res = statements.into_iter().map(
        |obj| {Stmt(obj, vec![])}).collect::<Vec<_>>();


    let mut unbounded_comments = vec![];

    for (line, comment) in comments {
        let mut best_distance = usize::MAX;
        let stmt_ref = find_best_match(&mut res, line, &mut best_distance);

        //believe me, i'm ashamed
        let stmt_ref = unsafe {stmt_ref.as_mut()};

        //println!("binding {:?} to {:?}", comment, stmt_ref);

        if let Some(ptr) = stmt_ref {
            ptr.1.push(comment);
        }else{
            unbounded_comments.push((line, comment));
        }

    }

    //push trailing comments

    let last_line = max(
        res.last().map(|s| {
            match &s.0 {
                StmtBody::Nop(t) => {t.get_line()}
                StmtBody::Assign(t, _) => {t.get_line()}
                StmtBody::Print(t, _) => {t.get_line()}
                StmtBody::Goto(t, _) => {t.get_line()}
                StmtBody::ExprStmt(e) => {e.get_line()}
            }
        }).unwrap_or(0),
        unbounded_comments.last().map(|(a,_b)| {*a})
            .unwrap_or(0)
    )+1;

    let mut record = Stmt(StmtBody::Nop(Token::Semicolon(Index{ line: last_line, column: 0 })), Vec::new());

    for item in unbounded_comments {
        record.1.push(item.1);
    }

    res.push(record);

    //let res = dbg!(res);
    #[cfg(debug_assertions)]
    for stmt in &res {
        println!("{}", stmt)
    }
    #[cfg(debug_assertions)]
    println!("^^^^ you see this because you have debug enabled");

    Ok(res)

}

fn find_best_match(statements: &Vec<Stmt>, line: usize, best_distance: &mut usize) -> *mut Stmt {
    let mut res = std::ptr::null_mut();
    for stmt in statements {
        let stmt_body = &stmt.0;
        match stmt_body {

            StmtBody::Assign(token, expr)|
            StmtBody::Print(token, expr) => {
                if token.get_line() >= line &&  token.get_line() - line < *best_distance {
                    *best_distance = token.get_line() - line;
                    res = stmt as *const Stmt as *mut Stmt;
                }

                let before = *best_distance;
                let left_res = find_best_match_expr(expr, line, best_distance);
                if before!= *best_distance {
                    res = left_res as *mut Stmt;
                }
            }

            StmtBody::ExprStmt(expr) => {
                if expr.get_line()>=line && expr.get_line() - line < *best_distance {
                    *best_distance = expr.get_line() - line;
                    res = stmt as *const Stmt as *mut Stmt;
                }

                let before = *best_distance;
                let left_res = find_best_match_expr(expr, line, best_distance);
                if before!= *best_distance {
                    res = left_res as *mut Stmt;
                }
            }
            StmtBody::Nop(token) |
            StmtBody::Goto(token, ..) => {
                if token.get_line()>=line && token.get_line() - line< *best_distance {
                    *best_distance = token.get_line() - line;
                    res = stmt as *const Stmt as *mut Stmt;
                }
            }

        };
    }
    res
}

fn find_best_match_expr(expr: &Expr, line:usize, best_distance: &mut usize) -> *mut Stmt {
    let mut res = std::ptr::null_mut();

    match expr {
        Expr::Variable(_) => {}
        Expr::Number(_) => {}
        Expr::String(_) => {}
        Expr::BinaryExpression(_, left, right) => {
            let before = *best_distance;
            let left_res = find_best_match_expr(left, line, best_distance);
            if *best_distance!=before {
                res = left_res;
            }
            let before = *best_distance;
            let right_res = find_best_match_expr(right, line, best_distance);
            if *best_distance!=before {
                res = right_res;
            }
        }

        Expr::UnaryExpression(_, expr) => {
            let before = *best_distance;
            let left_res = find_best_match_expr(expr, line, best_distance);
            if *best_distance!=before {
                res = left_res;
            }
        }
        Expr::Call(target, args) => {
            let before = *best_distance;
            let left_res = find_best_match_expr(target, line, best_distance);
            if *best_distance!=before {
                res = left_res;
            }
            for item in args {
                let before = *best_distance;
                let left_res = find_best_match_expr(item, line, best_distance);
                if *best_distance!=before {
                    res = left_res;
                }
            }

        }
        Expr::FunctionDeclaration(_token, body, _args) => {
            let before = *best_distance;

            let left_res = find_best_match(body, line, best_distance);
            if *best_distance!=before {
                res = left_res;
            }

        }
    }

    res
}

fn parse_comment(input: &str, line:usize) -> StmtComment {
    //println!("parisng {} at {}",input, line);
    match input {
        x if x.starts_with("when") => {
            let expr_parser = parser::ExprParser::new();
            let r1 = input.find('$');
            let r2 = input.rfind('$');
            if let (Some(r1), Some(r2)) = (r1, r2) {
                if r2>r1 {
                    let sub = &input[(r1+1)..r2];
                    if let Ok((tokens,_)) = token::tokenize(sub, line) {

                        if let Ok(tree) = expr_parser.parse(tokens) {
                            return StmtComment::OnlyWhen(tree);
                        }
                    }
                }

            }
            println!("WARN: failed to parse only when comment `{}` at {}, treating it as `other`", input, line);
            StmtComment::Other(input.to_string())
        }

        "never" => StmtComment::Never,

        x if x.starts_with("otherwise when") => {
            let expr_parser = parser::ExprParser::new();
            let r1 = input.find('$');
            let r2 = input.rfind('$');
            if let (Some(r1), Some(r2)) = (r1, r2) {
                if r2>r1 {
                    let sub = &input[(r1+1)..r2];
                    if let Ok((tokens,_)) = token::tokenize(sub, line) {

                        if let Ok(tree) = expr_parser.parse(tokens) {
                            return StmtComment::OtherwiseWhen(tree);
                        }
                    }
                }

            }
            println!("WARN: failed to parse only when comment `{}` at {}, treating it as `other`", input, line);
            StmtComment::Other(input.to_string())
        }

        x if x.starts_with("assert") => {
            let expr_parser = parser::ExprParser::new();
            let r1 = input.find('$');
            let r2 = input.rfind('$');
            if let (Some(r1), Some(r2)) = (r1, r2) {
                if r2>r1 {
                    let sub = &input[(r1+1)..r2];
                    if let Ok((tokens,_)) = token::tokenize(sub, line) {

                        if let Ok(tree) = expr_parser.parse(tokens) {
                            return StmtComment::Assert(tree);
                        }
                    }
                }

            }
            println!("WARN: failed to parse what looked like assert comment `{}` at {}, treating it as `other`", input, line);
            StmtComment::Other(input.to_string())
        }

        "otherwise" => StmtComment::Otherwise,

        "and" => StmtComment::And,

        "conditionally" => StmtComment::Conditionally,

        "once" => StmtComment::Once(line),

        _ => StmtComment::Other(input.to_string())
    }


}