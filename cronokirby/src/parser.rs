use std::iter::Peekable;
use std::mem;

use crate::lexer::{Lexer, Token};

/// Code represents a snippet of actual code.
#[derive(Debug, PartialEq)]
pub struct Code(pub String);

/// DocumentChunk represent an individual chunk composing our document.
#[derive(Debug, PartialEq)]
pub enum DocumentChunk {
    /// A commented bit of code, which should be executed, but the result discarded.
    Comment(Code),
    /// A interpolated bit of code, which should be executed and inlined.
    Interpolate(Code),
    /// A raw chunk of document which doesn't need to be executed at all.
    Raw(String),
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    acc: String,
    produced: Option<DocumentChunk>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Lexer<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
            acc: String::new(),
            produced: None,
        }
    }

    fn comment(&mut self) -> Option<Code> {
        let mut acc = String::new();
        while let Some(peek) = self.tokens.peek() {
            match peek {
                Token::CommentClose => {
                    self.tokens.next();
                    return Some(Code(acc));
                }
                t => {
                    t.push_to(&mut acc);
                    self.tokens.next();
                }
            }
        }
        None
    }

    fn interpolate(&mut self) -> Code {
        let mut acc = String::new();
        while let Some(peek) = self.tokens.peek() {
            match peek {
                Token::Tick => {
                    self.tokens.next();
                    return Code(acc);
                }
                t => {
                    t.push_to(&mut acc);
                    self.tokens.next();
                }
            }
        }
        Code(acc)
    }

    fn take_raw(&mut self) -> Option<DocumentChunk> {
        if self.acc.is_empty() {
            None
        } else {
            let acc = mem::take(&mut self.acc);
            Some(DocumentChunk::Raw(acc))
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = DocumentChunk;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(chunk) = mem::take(&mut self.produced) {
                return Some(chunk);
            }

            let next = match self.tokens.next() {
                None => return self.take_raw(),
                Some(tok) => tok,
            };

            match next {
                // TODO: Report an error here
                Token::CommentClose => {}
                Token::CommentOpen => {
                    if let Some(code) = self.comment() {
                        self.produced = Some(DocumentChunk::Comment(code))
                    }
                    if let Some(chunk) = self.take_raw() {
                        return Some(chunk);
                    }
                }
                Token::Tick => {
                    self.produced = Some(DocumentChunk::Interpolate(self.interpolate()));
                    if let Some(chunk) = self.take_raw() {
                        return Some(chunk);
                    }
                }
                t => t.push_to(&mut self.acc)
            }
        }
    }
}
