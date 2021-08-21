use std::borrow::Cow;

use crate::data::Value;

pub type Span = std::ops::Range<usize>;
pub type ExprPtr<'a> = Box<Expr<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct FileAst<'a> {
    pub header_comments: Vec<HeaderComment<'a>>,
    pub code: Ast<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HeaderComment<'a> {
    pub name: Vec<&'a str>,
    pub body: Vec<Stmt<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommentBody<'a> {
    /// There was no comment.
    Empty,
    /// A non-executable body of text.
    Text(Cow<'a, str>),
    /// An executable block of code.
    Stmt(Stmt<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LineComment<'a> {
    pub body: CommentBody<'a>,
    // The statement this comment aims to augment.
    pub stmt: Option<Stmt<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast<'a> {
    pub statements: Vec<LineComment<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind<'a> {
    LetDecl(Cow<'a, str>, Option<ExprPtr<'a>>),
    ExprStmt(ExprPtr<'a>),
    Print(Vec<Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'a> {
    Literal(Value<'a>),
    Variable(Cow<'a, str>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub span: Span,
}
