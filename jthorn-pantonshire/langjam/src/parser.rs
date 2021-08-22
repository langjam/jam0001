use anyhow::Context;
use pest::Parser;

use crate::value;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct LangParser;

type Pair<'a> = pest::iterators::Pair<'a, Rule>;

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn line_no(&self, source: &str) -> usize {
        source[..self.start]
            .lines()
            .count()
    }
}

impl From<pest::Span<'_>> for Span {
    fn from(span: pest::Span<'_>) -> Self {
        Self {
            start: span.start(),
            end: span.end(),
        }
    }
}

pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub span: Span,
    pub name: String,
    pub body: Vec<Stmt>,
    pub params: Vec<String>,
}

#[derive(Debug)]
pub enum Stmt {
    Var(VarStmt),
    Loop(LoopStmt),
    Call(CallStmt),
    Cond(CondStmt),
    Cons(ConsStmt),
    Return(ReturnStmt),
}

#[derive(Debug)]
pub struct ArgsStmt {
    pub span: Span,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub struct Arg {
    pub span: Span,
    pub name: String,
}

#[derive(Debug)]
pub struct VarStmt {
    pub variable: String,
    pub value: Expr,
}

#[derive(Debug)]
pub struct LoopStmt {
    pub span: Span,
    pub loops: Expr,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct CallStmt {
    pub span: Span,
    pub function: String,
    pub args: Vec<Expr>,
    pub store: Option<String>,
}

#[derive(Debug)]
pub struct CondStmt {
    pub span: Span,
    pub cond: Expr,
    pub stmt1: Vec<Stmt>,
    pub stmt2: Vec<Stmt>
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub span: Span,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct ConsStmt {

    pub span: Span,
    pub name: String,
    pub expr: Expr

}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Value(value::Value),
    Variable(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    List(Vec<Expr>),
}

#[derive(Debug)]
pub enum BinaryOp {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Minus,
}

pub fn parse(source: &str) -> anyhow::Result<Program> {
    let program = LangParser::parse(Rule::program, source)
        .context("parse error")?
        .next()
        .context("empty program")?;

    let mut functions = Vec::new();

    for pair in program.into_inner() {
        match pair.as_rule() {
            Rule::semanticBody => {
                functions.extend(parse_semantic_body(pair).into_iter());
            },

            Rule::syntaxBody => todo!(),

            Rule::EOI => (),

            _ => unreachable!(),
        }
    }

    Ok(Program {
        functions,
    })
}

fn parse_semantic_body(semantic_body: Pair) -> Vec<Function> {
    let mut functions = Vec::new();

    let mut pairs = semantic_body.into_inner();

    functions.push({
        let function_pair = pairs.next().unwrap();
        let function_span = Span::from(function_pair.as_span());
        let mut pairs = function_pair.into_inner();
        let statements = parse_comment(pairs.next().unwrap());
        let function_name = pairs.next().unwrap().as_str().to_lowercase();
        let function_params = pairs
            .map(|ident| ident.as_str().to_owned())
            .collect::<Vec<_>>();

        Function {
            span: function_span,
            name: function_name,
            body: statements,
            params: function_params,
        }
    });

    if let Some(semantic_body) = pairs.next() {
        functions.extend(parse_semantic_body(semantic_body).into_iter());
    }

    functions
}


fn parse_comment(comment: Pair) -> Vec<Stmt> {
    comment.into_inner().map(parse_statement).collect()
}

fn parse_statement(pair: Pair) -> Stmt {
    let pair = pair.into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::varStmt => {
            let mut pairs = pair.into_inner();
            let expr = parse_expr(pairs.next().unwrap());
            let variable = pairs.next().unwrap().as_str().to_owned();
            Stmt::Var(VarStmt {
                variable,
                value: expr,
            })
        },

        Rule::consStmt => {
            let span = pair.as_span().into();
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str().to_owned();
            let expr = parse_expr(pairs.next().unwrap());
            Stmt::Cons(ConsStmt {
                span,
                name,
                expr,
            })
        },

        Rule::loopStmt => {
            let span = pair.as_span().into();
            let mut pairs = pair.into_inner();
            let stmts = parse_comment(pairs.next().unwrap());
            let loops = parse_expr(pairs.next().unwrap());
            Stmt::Loop(LoopStmt {
                span,
                loops,
                stmts,
            })
        },

        Rule::callStmt => {
            let span = pair.as_span().into();
            let mut pairs = pair.into_inner();
            let function = pairs.next().unwrap().as_str().to_lowercase();
            let function = match function.strip_suffix('s') {
                Some(name) => name.to_owned(),
                //TODO: return an error instead
                None => panic!(r#"call to function "{}" must end with an s"#, function),
            };

            let mut args = None;
            let mut store = None;

            for pair in pairs {
                match pair.as_rule() {
                    Rule::callArgs => {
                        args = Some(pair.into_inner().map(parse_expr).collect::<Vec<_>>());
                    },

                    Rule::callStore => {
                        store = Some(pair.into_inner().next().unwrap().as_str().to_owned());
                    },

                    _ => unreachable!(),
                }
            }

            Stmt::Call(CallStmt {
                span,
                function,
                args: args.unwrap_or_else(|| Vec::new()),
                store,
            })
        },

        Rule::condStmt => {
            let span = pair.as_span().into();
            let mut pairs = pair.into_inner();
            let condition = parse_expr(pairs.next().unwrap());
            let branch1 = parse_comment(pairs.next().unwrap());
            let branch2 = parse_comment(pairs.next().unwrap());
            Stmt::Cond(CondStmt {
                span,
                cond: condition,
                stmt1: branch1,
                stmt2: branch2,
            })
        },

        Rule::returnStmt => {
            let span = pair.as_span().into();
            let expr = parse_expr(pair.into_inner().next().unwrap());
            Stmt::Return(ReturnStmt {
                span,
                expr,
            })
        },

   
        _ => unreachable!(),
    }
}

fn parse_expr(pair: Pair) -> Expr {

    fn parse_list(pair: Pair) -> Expr {
        let span = Span::from(pair.as_span());
        let expr = Expr {
            span,
            kind: ExprKind::List(pair.into_inner().map(parse_expr).collect::<Vec<_>>())
        };
        expr

    }

    fn parse_equality(pair: Pair) -> Expr {
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let lhs = parse_comparison(pairs.next().unwrap());

        let mut expr = lhs;

        while let Some(op) = pairs.next() {
            let op = match op.as_rule() {
                Rule::opNeq => BinaryOp::Neq,
                Rule::opEq => BinaryOp::Eq,
                _ => unreachable!(),
            };

            let rhs = parse_comparison(pairs.next().unwrap());

            expr = Expr {
                span,
                kind: ExprKind::Binary(op, Box::new(expr), Box::new(rhs))
            };
        }

        expr
    }

    fn parse_comparison(pair: Pair) -> Expr {
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let lhs = parse_term_or(pairs.next().unwrap());
        
        let mut expr = lhs;

        while let Some(op) = pairs.next() {
            let op = match op.as_rule() {
                Rule::opGt => BinaryOp::Gt,
                Rule::opGe => BinaryOp::Ge,
                Rule::opLt => BinaryOp::Lt,
                Rule::opLe => BinaryOp::Le,
                _ => unreachable!(),
            };

            let rhs = parse_term_or(pairs.next().unwrap());

            expr = Expr {
                span,
                kind: ExprKind::Binary(op, Box::new(expr), Box::new(rhs))
            };
        }

        expr
    }

    fn parse_term_or(pair: Pair) -> Expr {
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let lhs = parse_factor_and(pairs.next().unwrap());
        
        let mut expr = lhs;

        while let Some(op) = pairs.next() {
            let op = match op.as_rule() {
                Rule::opSub => BinaryOp::Sub,
                Rule::opAdd => BinaryOp::Add,
                Rule::opOr => BinaryOp::Or,
                _ => unreachable!(),
            };

            let rhs = parse_factor_and(pairs.next().unwrap());

            expr = Expr {
                span,
                kind: ExprKind::Binary(op, Box::new(expr), Box::new(rhs))
            };
        }

        expr
    }

    fn parse_factor_and(pair: Pair) -> Expr {
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let lhs = parse_unary(pairs.next().unwrap());

        match pairs.next() {
            Some(op) => {
                let op = match op.as_rule() {
                    Rule::opDiv => BinaryOp::Div,
                    Rule::opMul => BinaryOp::Mul,
                    Rule::opAnd => BinaryOp::And,
                    _ => unreachable!(),
                };
    
                let rhs = parse_unary(pairs.next().unwrap());
    
                Expr {
                    span,
                    kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs))
                }
            },

            None => lhs,
        }
    }

    fn parse_unary(pair: Pair) -> Expr {
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let first = pairs.next().unwrap();

        match first.as_rule() {
            Rule::primary => parse_primary(first),
            _ => {
                let op = match first.as_rule() {
                    Rule::opNot => UnaryOp::Not,
                    Rule::opNeg => UnaryOp::Minus,
                    _ => unreachable!(),
                };

                let expr = parse_unary(pairs.next().unwrap());

                Expr {
                    span,
                    kind: ExprKind::Unary(op, Box::new(expr))
                }
            },
        }
    }

    fn parse_primary(pair: Pair) -> Expr {
        let pair = pair.into_inner().next().unwrap();
        let span = Span::from(pair.as_span());

        match pair.as_rule() {
            Rule::number => Expr {
                span,
                kind: ExprKind::Value(value::Value::Int(pair.as_str().parse().unwrap()))
            },

            Rule::litTrue => Expr {
                span,
                kind: ExprKind::Value(value::Value::Bool(true))
            },

            Rule::litFalse => Expr {
                span,
                kind: ExprKind::Value(value::Value::Bool(false))
            },

            Rule::string => Expr {
                span,
                kind: ExprKind::Value(value::Value::String(pair.as_str().strip_prefix("\"").unwrap().strip_suffix("\"").unwrap().to_owned()))
            },

            Rule::list => parse_list(pair),

            Rule::ident => Expr {
                span,
                kind: ExprKind::Variable(pair.as_str().to_owned())
            },

            Rule::expr => parse_expr(pair),

            Rule::equality => parse_equality(pair),

            _ => unreachable!(),
        } 
    }

    parse_equality(pair.into_inner().next().unwrap())
}
