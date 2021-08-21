use thiserror::Error;
use std::iter::Peekable;
use std::str::Chars;

use crate::token::{TokenList, Position, Token, TokenKind};

macro_rules! match_token {
    ($token: ident, $($key: expr => $value: expr), *) => {
        match $token {
        $(
            $key => Some($value),
        )*
            _ => None
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'l> {
    source: Peekable<Chars<'l>>,
    current: char,
    position: Position,
}

impl<'l> Lexer<'l> {

    pub fn new(source: &'l str) -> Self {
        Self {
            source: source.chars().peekable(),
            current: '\0',
            position: Position::new(1, 0)
        }
    }

    pub fn skip_whitespace(&mut self) {
        while is_whitespace(self.current) {
            self.read();
        }
    }

    pub fn read(&mut self) -> Option<char> {
        if let Some(c) = self.source.next() {
            self.current = c;

            if c == '\n' {
                self.position.line += 1;
                self.position.column = 0;
            } else {
                self.position.column += 1;
            }

            return Some(c);
        }

        self.current = '\0';
        None
    }

    pub fn match_number(&mut self) -> Token {
        let position = self.position;
        let mut buffer = String::new();

        while is_numeric(self.current) || self.current == '.' {
            buffer.push(self.current);
            self.read();
        }

        Token::new(TokenKind::Number(buffer.parse().unwrap()), position)
    }

    pub fn match_identifier(&mut self) -> Token {
        let position = self.position;
        let mut buffer = String::new();

        loop {
            buffer.push(self.current);

            if self.read().is_none() || ! is_identifier(self.current) {
                break;
            }
        }

        if let Some(tk) = get_keyword(&buffer) {
            Token::new(tk, position)
        } else {
            Token::new(TokenKind::Identifier(buffer), position)
        }
    }

    pub fn match_string(&mut self) -> Token {
        let position = self.position;
        let mut buffer = String::new();

        let mut is_escaping = false;

        while let Some(c) = self.read() {
            if is_escaping {
                buffer.push(match c {
                    't' => '\t',
                    'n' => '\n',
                    'r' => '\r',
                    _ => c,
                });

                is_escaping = false;

                continue;
            }

            if c == '\"' {
                break;
            }

            if c == '\\' {
                is_escaping = true;

                continue;
            }

            buffer.push(c);
        }

        self.read();

        Token::new(TokenKind::String(buffer), position)
    }

    pub fn match_directive(&mut self) -> Token {
        let position = self.position;
        let mut buffer = String::from(self.current);

        self.read();

        while ! is_directive(&buffer) {
            buffer.push(self.current);

            self.read();
        }

        Token::new(get_directive(&buffer).unwrap(), position)
    }

    pub fn match_symbol(&mut self) -> Token {
        let position = self.position;
        let buffer = String::from(self.current);

        self.read();

        if is_symbol(self.current) {
            let mut multi_buffer = buffer.clone();
            multi_buffer.push(self.current);

            if let Some(s) = get_symbol(&multi_buffer) {
                self.read();

                return Token::new(s, position);
            }
        }

        Token::new(get_symbol(&buffer).unwrap(), position)
    }
}

impl<'l> Iterator for Lexer<'l> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let c = self.current;

        if c == '\0' {
            return None;
        }

        Some(match true {
            _ if is_symbol(c) => self.match_symbol(),
            _ if is_directive_starter(c) => self.match_directive(),
            _ if is_string_wrapper(c) => self.match_string(),
            _ if is_identifier(c) => self.match_identifier(),
            _ if is_numeric(c) => self.match_number(),
            _ => return None,
        })
    }
}

fn is_identifier(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn get_keyword(keyword: &str) -> Option<TokenKind> {
    match_token! { keyword,
        "fn" => TokenKind::Fn,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "null" => TokenKind::Null,
        "if" => TokenKind::If,
        "else" => TokenKind::Else
    }
}

fn is_string_wrapper(c: char) -> bool {
    c == '"'
}

fn is_directive_starter(c: char) -> bool {
    c == '@'
}

fn is_directive(directive: &str) -> bool {
    get_directive(directive).is_some()
}

fn get_directive(directive: &str) -> Option<TokenKind> {
    Some(match directive {
        "@name" | "@description" | "@author" | "@version" => TokenKind::FileDirective((&directive[1..directive.len()]).to_string()),
        "@param" | "@return" | "@identifier" => TokenKind::DefinitionDirective((&directive[1..directive.len()]).to_string()),
        _ => return None
    })
}

fn is_symbol(c: char) -> bool {
    let s = String::from(c);

    get_symbol(s.as_str()).is_some()
}

fn get_symbol(symbol: &str) -> Option<TokenKind> {
    match_token! { symbol,
        "/*" => TokenKind::CommentStarter,
        "*/" => TokenKind::CommentTerminator,
        "*" => TokenKind::Asterisk,
        "/" => TokenKind::ForwardSlash,
        "{" => TokenKind::LeftBrace,
        "}" => TokenKind::RightBrace,
        "(" => TokenKind::LeftParen,
        ")" => TokenKind::RightParen,
        "," => TokenKind::Comma,
        "+" => TokenKind::Plus,
        "-" => TokenKind::Minus
    }
}

fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\r' | '\n' => true,
        _ => false
    }
}

fn is_numeric(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false
    }
}

pub fn generate(source: &str) -> Result<TokenList, LexerError> {
    let mut lexer = Lexer::new(source);
    lexer.read();

    let mut tokens: TokenList = Vec::new();

    while let Some(token) = lexer.next() {
        tokens.push(token);
    }

    tokens.push(Token::new(TokenKind::Eof, lexer.position));

    Ok(tokens)
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unexpected character {0} on line {1} column {2}.")]
    UnexpectedCharacter(char, usize, usize),
}