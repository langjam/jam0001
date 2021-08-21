use std::slice::Iter;
use std::mem::discriminant;
use thiserror::Error;

use crate::token::{Token, TokenKind};
use crate::ast::{Program, Statement, FileHeader};

#[derive(Debug, Clone)]
pub struct Parser<'p> {
    tokens: Iter<'p, Token>,
    current: Token,
    peek: Token,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: Iter<'p, Token>, current: Token, peek: Token) -> Self {
        Self { tokens, current, peek }
    }

    pub fn read(&mut self) {
        self.current = self.peek.clone();
        self.peek = if let Some(t) = self.tokens.next() {
            t.clone()
        } else {
            Token::none()
        };
    }

    pub fn read_file_header(&mut self) -> Result<Statement, ParserError> {
        self.expect_token_and_read(TokenKind::CommentStarter)?;
        self.expect_token_and_read(TokenKind::Asterisk)?;
        self.expect_token_and_read(TokenKind::FileDirective("name".to_owned()))?;

        let name = match self.expect_token_and_read(TokenKind::String("".to_owned()))? {
            Token { kind: TokenKind::String(s), .. } => s,
            _ => unreachable!()
        };

        let mut description: Option<String> = None;
        let mut author: Option<String> = None;

        while self.current_is(TokenKind::Asterisk) {
            self.expect_token_and_read(TokenKind::Asterisk)?;

            match self.current.kind.clone() {
                TokenKind::FileDirective(d) => {
                    let d_token = self.expect_token_and_read(TokenKind::FileDirective("".to_owned()))?;

                    let value = match self.expect_token_and_read(TokenKind::String("".to_owned()))? {
                        Token { kind: TokenKind::String(s), .. } => s,
                        _ => unreachable!()
                    };

                    match d.as_str() {
                        "author" => author = Some(value),
                        "description" => description = Some(value),
                        _ => return Err(ParserError::InvalidFileHeader(d, d_token.position.line)),
                    }
                },
                _ => todo!()
            };
        }

        self.expect_token_and_read(TokenKind::CommentTerminator)?;

        Ok(Statement::FileHeader(FileHeader { name, description, author }))
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        Ok(Statement::Empty)
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<Token, ParserError> {
        if self.current_is(kind.clone()) {
            Ok(self.current.clone())
        } else {
            Err(ParserError::UnexpectedToken(self.current.kind.clone(), self.current.position.line, kind))
        }
    }

    fn expect_token_and_read(&mut self, kind: TokenKind) -> Result<Token, ParserError> {
        let result = self.expect_token(kind)?;

        self.read();

        Ok(result)
    }

    fn peek_is(&self, kind: TokenKind) -> bool {
        discriminant(&self.peek.kind) == discriminant(&kind)
    }

    fn current_is(&self, kind: TokenKind) -> bool {
        discriminant(&self.current.kind) == discriminant(&kind)
    }

    pub fn next(&mut self) -> Result<Option<Statement>, ParserError> {
        if self.current.kind == TokenKind::Eof || self.current.kind == TokenKind::None {
            return Ok(None);
        }

        Ok(Some(self.parse_statement()?))
    }
}

pub fn parse(tokens: Iter<Token>) -> Result<Program, ParserError> {
    let mut parser = Parser::new(tokens, Token::none(), Token::none());
    let mut program: Program = Vec::new();

    parser.read();
    parser.read();

    program.push(parser.read_file_header()?);

    while let Some(s) = parser.next()? {
        program.push(s);
    }

    Ok(program)
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token {0:?} on line {1}, expected {2:?}.")]
    UnexpectedToken(TokenKind, u16, TokenKind),

    #[error("Invalid file header {0} on line {1}.")]
    InvalidFileHeader(String, u16),
}