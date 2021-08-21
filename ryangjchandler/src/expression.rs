use crate::token::TokenKind;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Priority {
    Lowest,
    Assign,
    Sum,
    Product,
    Call,
}

impl Priority {
    pub fn get_precedence(kind: TokenKind) -> Self {
        match kind {
            TokenKind::LeftParen => Self::Call,
            TokenKind::Plus | TokenKind::Minus => Self::Sum,
            TokenKind::Asterisk | TokenKind::ForwardSlash => Self::Product,
            TokenKind::Assign => Self::Assign,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    String(String),
    Number(f64),
    Bool(bool),
    Null,
    GetIdentifier(String),
    Call(Call),
    Infix(Box<Expression>, TokenKind, Box<Expression>),
    Assign(Assign),

    // TODO: Remove this nasty ass hack.
    Empty,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub target: Box<Expression>,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}