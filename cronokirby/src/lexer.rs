use std::mem;
use std::str::Chars;

use peekmore::{PeekMore, PeekMoreIterator};

/// Represents a single token produced by our lexer
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// <!--
    CommentOpen,
    /// -->
    CommentClose,
    /// `
    Tick,
    /// The \n character
    Newline,
    /// Everything else is just raw text.
    Raw(String),
}

impl Token {
    /// push_to appends the contents of this token to a string.
    pub fn push_to(&self, acc: &mut String) {
        match self {
            Token::CommentOpen => acc.push_str("<!--"),
            Token::CommentClose => acc.push_str("-->"),
            Token::Tick => acc.push('`'),
            Token::Newline => acc.push('\n'),
            Token::Raw(s) => acc.push_str(s),
        }
    }
}

/// A Lexer uses our source code to emit tokens.
#[derive(Debug)]
pub struct Lexer<'a> {
    /// The source code for our program.
    src: PeekMoreIterator<Chars<'a>>,
    /// The current position in our source code.
    pos: usize,
    /// Used to a accumulate a raw string token
    raw_acc: String,
    /// This may contain a buffered output token
    produced: Option<Token>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer with some source code.
    pub fn new(src: &'a str) -> Self {
        Lexer {
            src: src.chars().peekmore(),
            pos: 0,
            raw_acc: String::new(),
            produced: None,
        }
    }

    fn take_raw(&mut self) -> Option<Token> {
        if self.raw_acc.is_empty() {
            None
        } else {
            Some(Token::Raw(mem::take(&mut self.raw_acc)))
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tok) = mem::take(&mut self.produced) {
            return Some(tok);
        }

        loop {
            let next = match self.src.next() {
                None => return self.take_raw(),
                Some(c) => c,
            };

            let produced = match next {
                '\n' => Some(Token::Newline),
                '`' => if Some('`').as_ref() == self.src.peek() {
                    self.src.next();
                    if Some('`').as_ref() == self.src.peek() {
                        self.src.next();
                        self.raw_acc.push_str("``");
                        None
                    } else {
                        Some(Token::Tick)
                    }
                } else {
                    None
                }
                '<' => {
                    if self.src.peek_nth(0).map_or(false, |x| *x == '!')
                        && self.src.peek_nth(1).map_or(false, |x| *x == '-')
                        && self.src.peek_nth(2).map_or(false, |x| *x == '-')
                    {
                        self.src.next();
                        self.src.next();
                        self.src.next();

                        return Some(Token::CommentOpen);
                    } else {
                        None
                    }
                }
                '-' => {
                    if self.src.peek_nth(0).map_or(false, |x| *x == '-')
                        && self.src.peek_nth(1).map_or(false, |x| *x == '>')
                    {
                        self.src.next();
                        self.src.next();

                        Some(Token::CommentClose)
                    } else {
                        None
                    }
                }
                _ => None,
            };
            if let Some(tok) = produced {
                if let Some(raw) = self.take_raw() {
                    self.produced = Some(tok);
                    return Some(raw);
                } else {
                    return Some(tok);
                }
            }
            self.raw_acc.push(next);
        }
    }
}
