use crate::token::TokenKind;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Priority {
    Lowest,
    Call,
}

impl Priority {
    pub fn get_precedence(kind: TokenKind) -> Self {
        match kind {
            TokenKind::LeftParen => Self::Call,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    String(String),
    GetIdentifier(String),
    Call(Call),

    // TODO: Remove this nasty ass hack.
    Empty,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}