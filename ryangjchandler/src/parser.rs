use std::slice::Iter;
use std::mem::discriminant;
use thiserror::Error;

use crate::token::{Token, TokenKind};
use crate::ast::{Program, Statement, FileHeader, FunctionDefinition, DefinitionHeader, If, While, Var, Const};
use crate::expression::{Expression, Priority, Call, Assign};

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
        let mut version: Option<String> = None;

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
                        "version" => version = Some(value),
                        _ => return Err(ParserError::InvalidFileHeader(d, d_token.position.line)),
                    }
                },
                _ => todo!()
            };
        }

        self.expect_token_and_read(TokenKind::CommentTerminator)?;

        Ok(Statement::FileHeader(FileHeader { name, description, author, version }))
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current.kind {
            TokenKind::Fn => self.parse_fn(),
            TokenKind::CommentStarter => self.parse_definition_header(),
            TokenKind::If => self.parse_if(),
            TokenKind::While => self.parse_while(),
            TokenKind::Var => self.parse_var(),
            TokenKind::Const => self.parse_const(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_var(&mut self) -> Result<Statement, ParserError> {
        self.expect_token_and_read(TokenKind::Var)?;

        let init = self.expect_expression(Priority::Assign)?;

        Ok(Statement::Var(Var { init }))
    }

    pub fn parse_const(&mut self) -> Result<Statement, ParserError> {
        self.expect_token_and_read(TokenKind::Const)?;

        let init = self.expect_expression(Priority::Assign)?;

        Ok(Statement::Const(Const { init }))
    }

    pub fn parse_while(&mut self) -> Result<Statement, ParserError> {
        self.expect_token_and_read(TokenKind::While)?;

        let condition = self.expect_expression(Priority::Lowest)?;
        let then = self.parse_block()?;

        Ok(Statement::While(While {
            condition,
            then
        }))
    }

    pub fn expect_expression(&mut self, priority: Priority) -> Result<Expression, ParserError> {
        Ok(match self.parse_expression(priority)? {
            None => return Err(ParserError::ExpectedExpression(self.current.position.line)),
            Some(e) => e,
        })
    }

    pub fn parse_if(&mut self) -> Result<Statement, ParserError> {
        self.expect_token_and_read(TokenKind::If)?;

        let condition = self.expect_expression(Priority::Lowest)?;
        let then = self.parse_block()?;
        let mut otherwise = Vec::new();

        if self.current_is(TokenKind::Else) {
            self.expect_token_and_read(TokenKind::Else)?;

            otherwise = self.parse_block()?;
        }

        Ok(Statement::If(If {
            condition,
            then,
            otherwise
        }))
    }

    pub fn parse_definition_header(&mut self) -> Result<Statement, ParserError>
    {
        self.expect_token_and_read(TokenKind::CommentStarter)?;
        self.expect_token_and_read(TokenKind::Asterisk)?;
        self.expect_token_and_read(TokenKind::DefinitionDirective("identifier".to_owned()))?;

        let identifier = match self.expect_token_and_read(TokenKind::Identifier("".to_owned()))? {
            Token { kind: TokenKind::Identifier(i), .. } => i,
            _ => unreachable!()
        };

        let mut params = Vec::new();
        let mut r#type: Option<String> = None;

        while self.current_is(TokenKind::Asterisk) {
            self.expect_token_and_read(TokenKind::Asterisk)?;

            match self.current.kind.clone() {
                TokenKind::DefinitionDirective(d) => {
                    self.expect_token_and_read(TokenKind::DefinitionDirective("".to_owned()))?;

                    match d.as_str() {
                        "param" => {
                            let identifier = match self.expect_token_and_read(TokenKind::Identifier("".to_string()))? {
                                Token { kind: TokenKind::Identifier(i), .. } => i,
                                _ => unreachable!()
                            };

                            let param_type = match self.current_is(TokenKind::Identifier("".to_string())) {
                                true => {
                                    Some(match self.expect_token_and_read(TokenKind::Identifier("".to_string()))? {
                                        Token { kind: TokenKind::Identifier(i), .. } => i,
                                        _ => unreachable!()
                                    })
                                },
                                false => None
                            };

                            params.push((identifier, param_type))
                        },
                        "type" => {
                            let identifier = match self.expect_token_and_read(TokenKind::Identifier("".to_string()))? {
                                Token { kind: TokenKind::Identifier(i), .. } => i,
                                _ => unreachable!()
                            };

                            r#type = Some(identifier)
                        },
                        _ => todo!()
                    }
                },
                _ => unreachable!()
            }
        }

        self.expect_token_and_read(TokenKind::CommentTerminator)?;

        Ok(Statement::DefinitionHeader(DefinitionHeader {
            identifier,
            params,
            r#type,
        }))
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement, ParserError>
    {
        if let Some(e) = self.parse_expression(Priority::Lowest)? {
            Ok(Statement::Expression(e))
        } else {
            Ok(Statement::Expression(Expression::Empty))
        }
    }

    pub fn parse_infix_expression(&mut self, left: Expression) -> Result<Option<Expression>, ParserError> {
        let operator = self.current.kind.clone();

        Ok(match operator {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Asterisk | TokenKind::ForwardSlash => {
                self.expect_token_and_read(operator.clone())?;

                let precedence = Priority::get_precedence(operator.clone());

                let right = match self.parse_expression(precedence)? {
                    Some(e) => e,
                    None => return Err(ParserError::ExpectedExpression(self.current.position.line))
                };

                Some(Expression::Infix(
                    Box::new(left),
                    operator,
                    Box::new(right),
                ))
            },
            TokenKind::Assign => {
                self.expect_token_and_read(TokenKind::Assign)?;

                let right = match self.parse_expression(Priority::Assign)? {
                    Some(e) => e,
                    None => return Err(ParserError::ExpectedExpression(self.current.position.line)),
                };

                Some(Expression::Assign(Assign {
                    target: Box::new(left),
                    value: Box::new(right),
                }))
            },
            _ => None,
        })
    }

    pub fn parse_postfix_expression(&mut self, left: Expression) -> Result<Option<Expression>, ParserError> {
        Ok(match self.current.kind.clone() {
            TokenKind::LeftParen => {
                self.expect_token_and_read(TokenKind::LeftParen)?;

                let mut args: Vec<Expression> = Vec::new();

                while ! self.current_is(TokenKind::RightParen) {
                    if let Some(e) = self.parse_expression(Priority::Lowest)? {
                        args.push(e);

                        if self.current_is(TokenKind::Comma) {
                            self.expect_token_and_read(TokenKind::Comma)?;
                        }
                    }
                }

                self.expect_token_and_read(TokenKind::RightParen)?;

                Some(Expression::Call(Call {
                    function: Box::new(left),
                    args
                }))
            },
            _ => None,
        })
    }

    pub fn parse_expression(&mut self, precedence: Priority) -> Result<Option<Expression>, ParserError>
    {
        Ok(if let Some(e) = self.parse_expression_starter()? {
            let mut left = e;

            while precedence < Priority::get_precedence(self.current.kind.clone()) {
                let e = left.clone();

                if let Some(right) = self.parse_postfix_expression(e.clone())? {
                    left = right;
                } else if let Some(right) = self.parse_infix_expression(e.clone())? {
                    left = right;
                } else {
                    break;
                }
            }

            Some(left)
        } else {
            None
        })
    }

    pub fn parse_expression_starter(&mut self) -> Result<Option<Expression>, ParserError> {
        Ok(match self.current.kind.clone() {
            TokenKind::String(s) => {
                self.expect_token_and_read(TokenKind::String("".to_owned()))?;

                Some(Expression::String(s))
            },
            TokenKind::Number(n) => {
                self.expect_token_and_read(TokenKind::Number(0.0))?;

                Some(Expression::Number(n))
            },
            TokenKind::True => {
                self.expect_token_and_read(TokenKind::True)?;

                Some(Expression::Bool(true))
            },
            TokenKind::False => {
                self.expect_token_and_read(TokenKind::False)?;

                Some(Expression::Bool(false))
            },
            TokenKind::Null => {
                self.expect_token_and_read(TokenKind::Null)?;

                Some(Expression::Null)
            },
            TokenKind::Identifier(i) => {
                self.expect_token_and_read(TokenKind::Identifier("".to_owned()))?;

                Some(Expression::GetIdentifier(i))
            },
            _ => None,
        })
    }

    pub fn parse_fn(&mut self) -> Result<Statement, ParserError>
    {
        self.expect_token_and_read(TokenKind::Fn)?;
        
        let body = self.parse_block()?;

        Ok(Statement::FunctionDefinition(FunctionDefinition { body }))
    }

    pub fn parse_block(&mut self) -> Result<Vec<Statement>, ParserError>
    {
        self.expect_token_and_read(TokenKind::LeftBrace)?;

        let mut block = Vec::new();

        while ! self.current_is_any_of(&[TokenKind::RightBrace]) {
            block.push(self.parse_statement()?);
        }

        self.expect_token_and_read(TokenKind::RightBrace)?;

        Ok(block)
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

    fn current_is_any_of(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.current_is(kind.clone()) {
                return true
            }
        }

        false
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

    #[error("Expected expression on line {0}.")]
    ExpectedExpression(u16),
}