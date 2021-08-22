use std::{iter::Peekable, str::Chars};

/// Represents a Token produced by our lexer.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// The `is` keyword
    Is,
    /// The `nil` keyword
    Nil,
    /// (
    OpenParens,
    /// )
    CloseParens,
    /// An identifier
    Identifier(String),
    // A signed integer
    Int(i64),
}

/// A lexer produces tokens as an iterator.
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars().peekable(),
        }
    }

    fn continue_int_lit(&mut self, start: char) -> i64 {
        let mut acc: i64 = start.to_digit(10).unwrap() as i64;
        while let Some(&peek) = self.chars.peek() {
            match peek.to_digit(10) {
                None => break,
                Some(d) => {
                    self.chars.next();
                    acc = 10 * acc + d as i64
                }
            }
        }
        acc
    }

    fn continue_identifier(&mut self, start: char) -> String {
        let mut ident = String::from(start);
        while let Some(&peek) = self.chars.peek() {
            if !(peek.is_alphanumeric() || peek == '_') {
                break;
            }
            self.chars.next();
            ident.push(peek);
        }
        ident
    }

    fn skip_whitespace(&mut self) {
        // UNSTABLE: you could use `next_if` here when it stabilizes
        while self.chars.next_if(|c| c.is_whitespace()).is_some() {}
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let next = match self.chars.next() {
            None => return None,
            Some(c) => c,
        };

        match next {
            '(' => return Some(Ok(Token::OpenParens)),
            ')' => return Some(Ok(Token::CloseParens)),
            c if c.is_digit(10) => {
                let lit = self.continue_int_lit(c);
                return Some(Ok(Token::Int(lit)));
            }
            c if c.is_alphabetic() || "_+-/*<=>".contains(c) => {
                let ident = self.continue_identifier(c);
                let tok = match ident.as_str() {
                    "is" => Token::Is,
                    "nil" => Token::Nil,
                    _ => Token::Identifier(ident),
                };
                return Some(Ok(tok));
            }
            c => return Some(Err(format!("unexpected character: `{}`", c))),
        }
    }
}
