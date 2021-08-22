mod error;
mod grammar;
mod lexer;
use std::iter::Peekable;

pub use lexer::*;

use crate::{ast::AST, parser::error::ParseError, T};

pub type Result<T> = std::result::Result<T, error::ParseError>;

pub fn parse_input(input: &str, name: impl Into<String>) -> Result<AST> {
    let mut parser = Parser::new(input, lex(input));
    parser.parse_input(name)
}

pub struct Parser<'input, I>
where
    I: Iterator,
{
    input: &'input str,
    lexer: Peekable<I>,
    pub(crate) allow_object_literals: bool,
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
{
    pub fn new(input: &'input str, lexer: I) -> Self {
        Self {
            input,
            lexer: lexer.peekable(),
            allow_object_literals: true,
        }
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// Returns the [`TokenKind`] of the next token, or `T![eof]` if at the end of input.
    pub(crate) fn peek(&mut self) -> TokenKind {
        self.lexer.peek().map(|token| token.kind).unwrap_or(T![eof])
    }

    /// Returns the string text of the next token, or an empty string if at the end of input.
    pub(crate) fn text(&mut self) -> &str {
        &self.input[self.position()]
    }

    /// Returns the [`Span`] of the next token, or an empty span at byte 0 if at the end of input.
    pub(crate) fn position(&mut self) -> Span {
        let peek = self.lexer.peek().map(|token| token.span);
        peek.unwrap_or_else(|| (0..0).into())
    }

    /// If the next token is of kind `expected`, advances past it and returns `true`.
    /// Otherwise, does not advance and returns `false`.
    pub(crate) fn try_consume(&mut self, expected: TokenKind) -> bool {
        if !self.at(expected) {
            return false;
        }
        self.bump();
        true
    }

    /// Tries to advance past a token of kind `expected`.
    /// If the next token is of a different kind, returns [`ParseError::UnexpectedToken`].
    pub(crate) fn consume(&mut self, expected: TokenKind) -> Result<TokenKind> {
        if self.try_consume(expected) {
            return Ok(expected);
        }
        Err(ParseError::UnexpectedToken {
            found: self.peek(),
            expected: vec![expected],
            position: self.position(),
        })
    }

    /// Returns `true` if the next token is of kind `kind`.
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Unconditionally advances the lexer by one token.
    pub(crate) fn bump(&mut self) {
        self.lexer.next();
    }
}
