use crate::token::TokenKind;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Priority {
    Lowest,
    Sum,
    Call,
}

impl Priority {
    pub fn get_precedence(kind: TokenKind) -> Self {
        match kind {
            TokenKind::LeftParen => Self::Call,
            TokenKind::Plus => Self::Sum,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    String(String),
    Number(f64),
    GetIdentifier(String),
    Call(Call),
    Infix(Box<Expression>, TokenKind, Box<Expression>),

    // TODO: Remove this nasty ass hack.
    Empty,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}