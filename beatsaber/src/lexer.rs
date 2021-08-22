use logos::{Logos, Span};
use std::fmt;

use crate::error::{Diagnostic, Label, Reporter};

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum Token {
    #[token(".")]
    Operator,
    #[token("//")]
    BehaviourStart,
    #[token("is")]
    Is,
    #[token("then")]
    Then,
    #[token("with")]
    With,
    #[token("if")]
    If,
    #[token("goto")]
    Goto,
    #[token("and")]
    And,
    #[regex("yeet|fuckall")]
    Discard,
    #[token("not here")]
    NotHere,
    #[token("but is in")]
    ButIsIn,
    #[token("this is big")]
    ThisIsBig,
    #[token("return")]
    Return,
    #[token("(")]
    ParenLeft,
    #[token(")")]
    ParenRight,
    #[token("still in")]
    StillIn,
    #[regex(r"\n|\f")]
    Newline,
    #[regex(r"[A-Za-z_][A-Za-z_0-9]*")]
    Identifier,
    #[regex(r"\d+", |lex| lex.slice().parse())]
    #[regex("'.'", |lex| usize::from(lex.slice().as_bytes()[1]))]
    Number(usize),
    #[regex(r#""(\\.|[^"\\])*""#)]
    StringLiteral,

    #[regex(r"[ \t\r]", logos::skip)]
    #[regex(r"\*.*", logos::skip)]
    #[error]
    Error,
}

pub struct Lexer<'a> {
    inner: logos::SpannedIter<'a, Token>,
    peeked: Option<Option<(Token, Span)>>,
    reporter: Reporter<'a>,
    src: &'a str,
}

impl Iterator for Lexer<'_> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return next;
        }
        self.inner.next()
    }
}

impl<'a> Lexer<'a> {
    pub fn peek(&mut self) -> Option<(Token, Span)> {
        if let Some(peeked) = &mut self.peeked {
            return peeked.clone();
        }
        self.peeked.insert(self.inner.next()).clone()
    }

    pub fn monch(&mut self, token: Token) -> Span {
        let (t, span) = self.next().unwrap();
        if t != token {
            self.reporter.report_and_exit(
                &Diagnostic::error()
                    .with_message("unexpected token")
                    .with_labels(vec![Label::primary((), span)
                        .with_message(format!("expected token `{}` here", token))]),
            )
        }
        span
    }

    pub fn reporter(&self) -> Reporter<'a> {
        self.reporter.clone()
    }

    pub fn src(&self) -> &'a str {
        self.src
    }
}

pub fn lexer<'a>(src: &'a str, file: &'a str) -> Lexer<'a> {
    Lexer {
        inner: Token::lexer(src).spanned(),
        peeked: None,
        reporter: Reporter::new(src, file),
        src,
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Operator => ".",
            Self::BehaviourStart => "//",
            Self::Is => "is",
            Self::Then => "then",
            Self::With => "with",
            Self::If => "if",
            Self::Goto => "goto",
            Self::Discard => "<discard>",
            Self::NotHere => "not here",
            Self::Return => "return",
            Self::ParenLeft => "(",
            Self::ParenRight => ")",
            Self::StillIn => "still in",
            Self::And => "and",
            Self::Newline => "<newline>",
            Self::Identifier => "<identifier>",
            Self::Number(_) => "<number>",
            Self::StringLiteral => "<string>",
            _ => "<unknown>",
        };
        f.write_str(s)
    }
}

#[cfg(test)]
mod tests {
    use super::{Logos, Token};

    #[test]
    fn simple_lex() {
        let mut lex = Token::lexer("a. // yeet is increment\n");
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "a");
        assert_eq!(lex.next(), Some(Token::Operator));
        assert_eq!(lex.slice(), ".");
        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.slice(), "//");
        assert_eq!(lex.next(), Some(Token::Discard));
        assert_eq!(lex.slice(), "yeet");
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.slice(), "is");
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "increment");
        assert_eq!(lex.next(), Some(Token::Newline));
    }

    #[test]
    fn complex_lex() {
        let mut lex = Token::lexer(
            r"* Recursive fibonacci to get the nth number in the sequence ****
// fib is with n
// still in fib one is 1
// still in fib two is 2
n.two // still in fib cond is less
n // still in fib if cond return is
(n.one)..(n.two). // still in fib return is sub then fib then sub then fib then add
// malloc is not here
// multiply is with a and b
",
        );
        // Comments are ignored, the \n after a comment turns into a newline
        assert_eq!(lex.next(), Some(Token::Newline));
        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::With));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "n");
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::StillIn));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "fib");
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "one");
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::Number(1)));
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::StillIn));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "fib");
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "two");
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::Number(2)));
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "n");
        assert_eq!(lex.next(), Some(Token::Operator));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "two");
        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::StillIn));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "fib");
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "cond");
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "less");
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "n");
        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::StillIn));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "fib");
        assert_eq!(lex.next(), Some(Token::If));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "cond");
        assert_eq!(lex.next(), Some(Token::Return));
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::ParenLeft));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "n");
        assert_eq!(lex.next(), Some(Token::Operator));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "one");
        assert_eq!(lex.next(), Some(Token::ParenRight));
        assert_eq!(lex.next(), Some(Token::Operator));
        assert_eq!(lex.next(), Some(Token::Operator));
        assert_eq!(lex.next(), Some(Token::ParenLeft));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "n");
        assert_eq!(lex.next(), Some(Token::Operator));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "two");
        assert_eq!(lex.next(), Some(Token::ParenRight));
        assert_eq!(lex.next(), Some(Token::Operator));
        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::StillIn));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "fib");
        assert_eq!(lex.next(), Some(Token::Return));
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "sub");
        assert_eq!(lex.next(), Some(Token::Then));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "fib");
        assert_eq!(lex.next(), Some(Token::Then));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "sub");
        assert_eq!(lex.next(), Some(Token::Then));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "fib");
        assert_eq!(lex.next(), Some(Token::Then));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "add");
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "malloc");
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::NotHere));
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::BehaviourStart));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "multiply");
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::With));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "a");
        assert_eq!(lex.next(), Some(Token::And));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.slice(), "b");
    }
}
