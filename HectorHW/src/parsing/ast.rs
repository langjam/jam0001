#[derive(Debug, Clone, PartialEq)]
pub struct Stmt (pub StmtBody, pub Vec<StmtComment>);

pub type Program = Vec<Stmt>;

use super::token::{Token};
use std::fmt::{Display, Formatter, Debug};

#[derive(Debug, Clone, PartialEq)]
pub enum StmtBody {
    Nop(Token),
    Assign(Token, Expr),
    Print(Token, Expr),
    Goto(Token, String),
    ExprStmt(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtComment {
    OnlyWhen(Expr),
    Never,
    Otherwise,
    Once(usize),
    OtherwiseWhen(Expr),
    Assert(Expr),
    And,
    Conditionally,
    Other(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Variable(Token),
    Number(Token),
    String(Token),
    BinaryExpression(Token, Box<Expr>, Box<Expr>),
    UnaryExpression(Token, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    FunctionDeclaration(Token, Vec<Stmt>, Vec<Token>)
}

impl Expr {
    #[allow(dead_code)]
    pub fn unwrap_string(self) -> Option<String> {
        match self {
            Expr::Variable(v) => {
                v.unwrap_string()
            }
            _ => None
        }
    }

    pub fn get_line(&self) -> usize {
        match self {
            Expr::Variable(v) => {v.get_line()}
            Expr::Number(n) => {n.get_line()}
            Expr::String(s) => {s.get_line()}
            Expr::BinaryExpression(_op, left, _) => {left.get_line()}
            Expr::UnaryExpression(op, _) => {op.get_line()}
            Expr::Call(target, _) => {target.get_line()}
            Expr::FunctionDeclaration(token, _, _) => {token.get_line()}
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Variable(v) => {write!(f, "{}", v)}
            Expr::Number(n) => {write!(f, "{}", n)}
            Expr::String(s) => {write!(f, "{}", s)}
            Expr::BinaryExpression(op, left, right) => {
                write!(f, "({} {} {})", left, op, right)
            }
            Expr::UnaryExpression(op, target) => {
                write!(f, "({} {})", op, target)
            }
            Expr::Call(target, args) => {
                write!(f, "{}(", target)?;
                let args =args.iter().map(|a| {format!("{}", a)}).collect::<Vec<_>>();
                    let args = args.join(", ");
                write!(f, "{}", args)?;
                write!(f, ")")
            }
            Expr::FunctionDeclaration(function_token, body, args) => {
                write!(f, "{}(", function_token)?;
                let args =args.iter().map(|a| {format!("{}", a)}).collect::<Vec<_>>();
                let args = args.join(", ");
                writeln!(f, "{}) (", args)?;
                for stmt in body {
                    writeln!(f, "{}", stmt)?;
                }
                writeln!(f, ")")
            }
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for StmtBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtBody::Nop(_) => {
                write!(f, "nop;")
            }
            StmtBody::Assign(target, expr) => {
                write!(f, "{} = {};", target, expr)
            }
            StmtBody::Print(kw, arg) => {
                write!(f, "{} {};", kw, arg)
            }
            StmtBody::Goto(kw, trg) => {
                write!(f, "{} {};", kw, trg)
            }
            StmtBody::ExprStmt(expr) => {
                write!(f, "{};", expr)
            }
        }
    }
}