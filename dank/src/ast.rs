use std::{borrow::Cow, cell::RefCell, rc::Rc};

use ast2str::AstToStr;
use enum_kinds::EnumKind;

use crate::data::Value;

pub type Span = std::ops::Range<usize>;
pub type Ptr<T> = Box<T>;
pub type ExprPtr<'a> = Ptr<Expr<'a>>;
pub type StmtPtr<'a> = Ptr<Stmt<'a>>;

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct HeaderComment<'a> {
    pub name: Vec<&'a str>,
    pub body: Vec<Stmt<'a>>,
}

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub enum CommentBody<'a> {
    /// There was no comment.
    Empty,
    /// A non-executable body of text.
    Text(#[rename = "text"] Cow<'a, str>),
    /// An executable block of code.
    Stmt(#[rename = "stmt"] Stmt<'a>),
}

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct LineComment<'a> {
    pub body: CommentBody<'a>,
    // The statement this comment aims to augment.
    pub stmt: Option<Stmt<'a>>,
}

// TODO: closure captures
#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct Function<'a> {
    pub name: Cow<'a, str>,
    pub args: Vec<Cow<'a, str>>,
    pub body: Stmt<'a>,
}

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct Ast<'a> {
    pub statements: Vec<LineComment<'a>>,
}

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct If<'a> {
    pub branches: Vec<(Expr<'a>, Stmt<'a>)>,
    pub otherwise: Option<Stmt<'a>>,
}

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct Block<'a> {
    pub statements: Vec<LineComment<'a>>,
}

#[derive(EnumKind, Debug, Clone, PartialEq, AstToStr)]
#[enum_kind(StmtKindName)]
pub enum StmtKind<'a> {
    LetDecl(
        #[rename = "name"] Cow<'a, str>,
        #[rename = "initializer"] Option<ExprPtr<'a>>,
    ),
    FuncDecl(#[forward] Ptr<Function<'a>>),
    ExprStmt(#[rename = "expr"] ExprPtr<'a>),
    Print(#[rename = "args"] Vec<Expr<'a>>),
    Block(#[forward] Block<'a>),
    UnscopedBlock(#[rename = "statements"] Vec<LineComment<'a>>),
    If(Ptr<If<'a>>),
    While(
        #[rename = "condition"] ExprPtr<'a>,
        #[rename = "body"] StmtPtr<'a>,
    ),
    Return(#[rename = "value"] Option<ExprPtr<'a>>),
    Assignment(
        #[rename = "name"] Cow<'a, str>,
        #[rename = "value"] ExprPtr<'a>,
    ),
    PropAssignment(
        #[rename = "obj"] ExprPtr<'a>,
        #[rename = "prop"] Cow<'a, str>,
        #[rename = "value"] ExprPtr<'a>,
    ),
    DynPropAssignment(
        #[rename = "obj"] ExprPtr<'a>,
        #[rename = "key"] ExprPtr<'a>,
        #[rename = "value"] ExprPtr<'a>,
    ),
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq, AstToStr)]
pub enum BinOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Or,
    And,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl BinOpKind {
    pub fn symbol(&self) -> &'static str {
        match self {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Div => "/",
            BinOpKind::Mul => "*",
            BinOpKind::Or => "||",
            BinOpKind::And => "&&",
            BinOpKind::Eq => "==",
            BinOpKind::Ne => "!=",
            BinOpKind::Lt => "<",
            BinOpKind::Le => "<=",
            BinOpKind::Gt => ">",
            BinOpKind::Ge => ">=",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, AstToStr)]
pub enum UnOpKind {
    Neg,
    Not,
}

impl UnOpKind {
    pub fn symbol(&self) -> &'static str {
        match self {
            UnOpKind::Neg => "-",
            UnOpKind::Not => "!",
        }
    }
}

#[derive(EnumKind, Debug, Clone, PartialEq, AstToStr)]
#[enum_kind(ExprKindName)]
pub enum ExprKind<'a> {
    ObjectLiteral(Vec<(Cow<'a, str>, Expr<'a>)>),
    LambdaLiteral(Ptr<Function<'a>>),
    Literal(#[debug] Value<'a>),
    Variable(#[rename = "name"] Cow<'a, str>),
    Property(
        #[rename = "name"] Cow<'a, str>,
        #[rename = "obj"] ExprPtr<'a>,
    ),
    DynProperty(#[rename = "key"] ExprPtr<'a>, #[rename = "obj"] ExprPtr<'a>),
    Call(
        #[rename = "callee"] ExprPtr<'a>,
        #[rename = "args"] Vec<Expr<'a>>,
    ),
    Binary(
        #[rename = "left"] ExprPtr<'a>,
        #[rename = "op"] BinOpKind,
        #[rename = "right"] ExprPtr<'a>,
    ),
    Unary(#[rename = "op"] UnOpKind, #[rename = "operand"] ExprPtr<'a>),
}

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct Expr<'a> {
    #[forward]
    pub kind: Rc<RefCell<ExprKind<'a>>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, AstToStr)]
pub struct Stmt<'a> {
    #[forward]
    pub kind: Rc<RefCell<StmtKind<'a>>>,
    pub span: Span,
}

impl<'a> StmtKind<'a> {
    pub fn alloc(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }
}
impl<'a> ExprKind<'a> {
    pub fn alloc(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }
}
