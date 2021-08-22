use anyhow::{anyhow, bail};
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

#[derive(Debug)]
pub struct Program {
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<BlockEl>);

impl Block {
    pub fn exprs(&self) -> impl Iterator<Item = &Expr> + '_ {
        self.0.iter().filter_map(|block_el| match block_el {
            BlockEl::Expr(expr) => Some(expr),
            BlockEl::NewLine => None,
        })
    }

    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut Expr> + '_ {
        self.0.iter_mut().filter_map(|block_el| match block_el {
            BlockEl::Expr(expr) => Some(expr),
            BlockEl::NewLine => None,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockEl {
    Expr(Expr),
    NewLine,
}

// TODO: should probably put a concept of newline into here because newlines from the programmer
// are important
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Block(Block),
    Ref(Ref),
    Comment(Comment),
    Assignment(Assignment),
    IntLiteral(i128),
    FuncDef(FuncDef),
    FunctionCall(FunctionCall),
    While(While),
    If(If),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub arg_names: Vec<String>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comment {
    pub name: Option<String>,
    pub body: String,
}

pub fn find_comments_mut(
    program: &'a mut Program,
) -> anyhow::Result<HashMap<String, &'a mut Comment>> {
    let mut comments = HashMap::new();
    for expr in &mut program.block.exprs_mut() {
        try_extend(&mut comments, &mut find_expr_comments_mut(expr)?)?;
    }
    Ok(comments)
}

fn find_expr_comments_mut(expr: &'a mut Expr) -> anyhow::Result<HashMap<String, &'a mut Comment>> {
    let mut comments = HashMap::new();
    match expr {
        Expr::Block(block) => {
            for expr in block.exprs_mut() {
                try_extend(&mut comments, &mut find_expr_comments_mut(expr)?)?;
            }
        }
        Expr::Comment(c) => {
            let name = c.name.clone();
            if let Some(name) = name {
                try_insert(&mut comments, name, c)?;
            }
        }
        Expr::Assignment(Assignment { r#ref: _, expr }) => {
            try_extend(&mut comments, &mut find_expr_comments_mut(expr)?)?;
        }
        Expr::FunctionCall(FunctionCall { r#ref: _, args }) => {
            for expr in args {
                try_extend(&mut comments, &mut find_expr_comments_mut(expr)?)?;
            }
        }
        Expr::While(While { cond, block }) | Expr::If(If { cond, block }) => {
            try_extend(&mut comments, &mut find_expr_comments_mut(cond)?)?;
            for expr in block.exprs_mut() {
                try_extend(&mut comments, &mut find_expr_comments_mut(expr)?)?;
            }
        }
        Expr::Ref(_) | Expr::IntLiteral(_) => {}
        Expr::FuncDef(FuncDef {
            name: _,
            arg_names: _,
            block,
        }) => {
            for expr in block.exprs_mut() {
                try_extend(&mut comments, &mut find_expr_comments_mut(expr)?)?;
            }
        }
    }
    Ok(comments)
}

pub fn try_extend<K: Eq + Hash + Send + Sync + Debug + Display, V: Send + Sync + Debug>(
    into: &mut HashMap<K, &'a mut V>,
    from: &mut HashMap<K, &'a mut V>,
) -> anyhow::Result<()> {
    for (k, v) in from.drain() {
        try_insert(into, k, v)?;
    }
    Ok(())
}

fn try_insert<K: Eq + Hash + Send + Sync + Debug + Display, V: Send + Sync + Debug>(
    into: &mut HashMap<K, &'a mut V>,
    k: K,
    v: &'a mut V,
) -> anyhow::Result<()> {
    if into.contains_key(&k) {
        bail!(anyhow!("key {} already exists", k));
    }
    into.insert(k, v);
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ref {
    CommentRef(String),
    VarRef(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub r#ref: Ref,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub r#ref: Ref,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub cond: Box<Expr>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub block: Block,
}

// usage of peg stolen from https://github.com/A1Liu/gone/blob/master/src/parser.rs
peg::parser! {
    pub grammar parser() for str {
        pub rule program() -> Program
            = block:block()  { Program { block } }

        rule block() -> Block
            = block_els:(block_el()+) { Block(block_els) }

        rule block_el() -> BlockEl
            = nbspace()? b:(block_el_expr() / block_el_blankline()) { b }

        rule block_el_expr() -> BlockEl
            = e:expr() { BlockEl::Expr(e) }

        rule block_el_blankline() -> BlockEl
            = newline() { BlockEl::NewLine }

        rule func_decl() -> Expr
            = "defn" _? name:ident() _? "(" _? arg_names:(ident() ** comma()) _? ")" _* "{" _? block:block() _? "}" {
                Expr::FuncDef(FuncDef {
                    name: name.to_string(),
                    arg_names: arg_names.iter().map(|n| n.to_string()).collect(),
                    block,
                })
            }

        rule if_statement() -> Expr
            = "if" _? "(" _? cond:expr() _? ")" _* "{" _? block:block() _? "}" {
                Expr::If(If {
                    cond: Box::new(cond),
                    block,
                })
            }

        rule while_loop() -> Expr
            = "while" _? "(" _? cond:expr() _? ")" _* "{" _? block:block() _? "}" {
                Expr::While(While {
                    cond: Box::new(cond),
                    block,
                })
            }

        rule expr() -> Expr
            = while_loop() / if_statement() / func_decl() / comment() / assignment() / int() / func_call() / r#ref()

        rule func_call() -> Expr
            = r#ref:ref_ref() "(" _? args:(expr() ** comma()) _? ")" {
                Expr::FunctionCall(FunctionCall {
                    r#ref,
                    args,
                })
            }

        rule r#ref() -> Expr
            = r:ref_ref() { Expr::Ref(r) }
        rule ref_ref() -> Ref
            = var_ref() / comment_ref()
        rule var_ref() -> Ref
            = r:ident() { Ref::VarRef(r.into()) }
        rule comment_ref() -> Ref
            = r:comment_ident() { Ref::CommentRef(r) }
        rule comment_ident() -> String
            = "#" i:ident() { i.into() }

        rule assignment() -> Expr
            = "let" _ r:ref_ref() _ "=" _ expr:expr() { Expr::Assignment(Assignment {
                r#ref: r,
                expr: Box::new(expr),
            })}


        rule int() -> Expr
            = num:$("0" / "-"? ['1' ..= '9']+ ['0' ..= '9']*) { Expr::IntLiteral(num.parse().unwrap()) }

        rule comment() -> Expr = named_comment() / anon_comment()

        rule named_comment() -> Expr
            = "/" "/" _? name:comment_ident() body:following_comment()?  {
                Expr::Comment(Comment { name: Some(name), body: body.unwrap_or_else(|| "".into()) })
            }

        rule anon_comment() -> Expr
            = body:comment_string() { Expr::Comment(Comment { name: None, body })}

        rule comment_string() -> String
            = "/" "/" onespace()? body:$([^ '\r' | '\n']*)? following:following_comment()*  {
                body.map(|b| b.to_owned()).into_iter().chain(following.into_iter()).join("\n")
            }
        rule following_comment() -> String
            = newline() c:comment_string() {
                if c.starts_with("//") {
                    let c = c.trim_start_matches("//");
                    let c = c.strip_prefix(' ').unwrap_or(c);
                    format!("\n{}", c)
                } else {
                    c
                }
            }

        rule ident() -> &'input str = $(ident_start()+ ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
        rule ident_start() -> &'input str = $(['a'..='z' | 'A'..='Z' | '_']+)

        rule comma() -> () = _? "," _?
        rule nbspace() = onespace()+
        rule onespace() = [' ' | '\t']
        rule newline() = "\n" / "\r\n"
        rule whitespace() = (nbspace() / newline())+
        rule _() = quiet!{ whitespace() };
    }
}
