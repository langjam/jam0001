use std::ops::{Index, Range};

use logos::{Lexer, Logos};

pub fn lex(input: &str) -> impl Iterator<Item = Token> + '_ {
    TokenKind::lexer(input)
        .spanned()
        .filter(|(token, _)| !token.is_trivia())
        .map(|(token, span)| Token {
            kind: token,
            span: span.into(),
        })
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Default, Debug)]
pub struct Span {
    /// inclusive
    pub start: u32,
    /// exclusive
    pub end: u32,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl Index<Span> for str {
    type Output = str;
    fn index(&self, span: Span) -> &Self::Output {
        &self[Range::<usize>::from(span)]
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} - <{}, {}>",
            self.kind, self.span.start, self.span.end
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::Eof,
            span: Span { start: 0, end: 0 },
        }
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenKind {
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("^")]
    Pow,
    #[token("=")]
    Eq,
    #[token("!")]
    Bang,
    #[token("&")]
    Amp,
    #[token("|")]
    Bar,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("==")]
    Eqq,
    #[token("!=")]
    Neq,
    #[token(">=")]
    Geq,
    #[token("<=")]
    Leq,
    #[token("?")]
    Question,
    #[token("_")]
    Under,
    #[token("#")]
    Pound,
    #[token("$")]
    Dollar,
    #[token(r#"""#)]
    Quote,
    // Brackets
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    // Constructs
    #[token("::")]
    PathSep,
    #[token("->")]
    Arrow,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    String,
    #[regex(r#"'(\w| |\\[trnb\\']|"|[[:punct:]&&[^'\\]])'"#)]
    Char,
    #[regex(r#"//[^\r\n;]*"#)]
    LineComment,
    #[token("/*", block_comment)]
    BlockComment,
    #[token("true")]
    #[token("false")]
    Bool,
    #[regex(r#"\d+"#, priority = 2)]
    Int,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#)]
    Float,
    #[regex(r#"[A-Za-z]([A-Za-z]|_|\d)*"#)]
    Ident,

    // Keywords
    #[token("let")]
    KwLet,
    #[token("const")]
    KwConst,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("match")]
    KwMatch,
    #[token("while")]
    KwWhile,
    #[token("for")]
    KwFor,
    #[token("loop")]
    KwLoop,
    #[token("in")]
    KwIn,
    #[token("extern")]
    KwExtern,
    #[token("fn")]
    KwFn,
    #[token("return")]
    KwReturn,
    #[token("mod")]
    KwMod,
    #[token("class")]
    KwClass,
    #[token("ref")]
    KwRef,
    #[regex(r"[ \t\r\n\f]+")]
    Ws,
    #[error]
    Error,
    Eof,
}

fn block_comment(lexer: &mut Lexer<'_, TokenKind>) -> bool {
    assert_eq!(lexer.slice(), "/*");
    let mut level = 1u32;
    let mut rest = lexer.remainder().chars().peekable();
    while let Some(c) = rest.next() {
        match c {
            '*' => {
                if let Some('/') = rest.peek() {
                    rest.next();
                    level -= 1;
                    lexer.bump(2);
                    if level == 0 {
                        break;
                    }
                    continue;
                }
            }
            '/' => {
                if let Some('*') = rest.peek() {
                    rest.next();
                    level += 1;
                    lexer.bump(2);
                    continue;
                }
            }
            _ => {}
        }
        lexer.bump(1);
    }

    level == 0
}

#[macro_export]
macro_rules! T {
    [+] => {
        TokenKind::Plus
    };
    [-] => {
        TokenKind::Minus
    };
    [*] => {
        TokenKind::Times
    };
    [/] => {
        TokenKind::Slash
    };
    [%] => {
        TokenKind::Percent
    };
    [^] => {
        TokenKind::Pow
    };
    [=] => {
        TokenKind::Eq
    };
    [.] => {
        TokenKind::Dot
    };
    [,] => {
        TokenKind::Comma
    };
    [_] => {
        TokenKind::Under
    };
    [!] => {
        TokenKind::Bang
    };
    [&] => {
        TokenKind::Amp
    };
    [|] => {
        TokenKind::Bar
    };
    [&&] => {
        TokenKind::And
    };
    [||] => {
        TokenKind::Or
    };
    [==] => {
        TokenKind::Eqq
    };
    [!=] => {
        TokenKind::Neq
    };
    [<=] => {
        TokenKind::Leq
    };
    [>=] => {
        TokenKind::Geq
    };
    [?] => {
        TokenKind::Question
    };
    [:] => {
        TokenKind::Colon
    };
    [;] => {
        TokenKind::Semi
    };
    [#] => {
        TokenKind::Pound
    };
    [$] => {
        TokenKind::Dollar
    };
    ['"'] => {
        TokenKind::Quote
    };
    [<] => {
        TokenKind::LAngle
    };
    [>] => {
        TokenKind::RAngle
    };
    ['['] => {
        TokenKind::LSquare
    };
    [']'] => {
        TokenKind::RSquare
    };
    ['{'] => {
        TokenKind::LBrace
    };
    ['}'] => {
        TokenKind::RBrace
    };
    ['('] => {
        TokenKind::LParen
    };
    [')'] => {
        TokenKind::RParen
    };
    [::] => {
        TokenKind::PathSep
    };
    [->] => {
        TokenKind::Arrow
    };
    [string] => {
        TokenKind::String
    };
    [char] => {
        TokenKind::Char
    };
    [line comment] => {
        TokenKind::LineComment
    };
    [block comment] => {
        TokenKind::BlockComment
    };
    [bool] => {
        TokenKind::Bool
    };
    [int] => {
        TokenKind::Int
    };
    [float] => {
        TokenKind::Float
    };
    [ident] => {
        TokenKind::Ident
    };
    [let] => {
        TokenKind::KwLet
    };
    [const] => {
        TokenKind::KwConst
    };
    [if] => {
            TokenKind::KwIf
    };
    [else] => {
            TokenKind::KwElse
    };
    [match] => {
            TokenKind::KwMatch
    };
    [while] => {
            TokenKind::KwWhile
    };
    [for] => {
            TokenKind::KwFor
    };
    [loop] => {
            TokenKind::KwLoop
    };
    [in] => {
            TokenKind::KwIn
    };
    [extern] => {
        TokenKind::KwExtern
    };
    [fn] => {
        TokenKind::KwFn
    };
    [return] => {
        TokenKind::KwReturn
    };
    [mod] => {
        TokenKind::KwMod
    };
    [class] => {
        TokenKind::KwClass
    };
    [ref] => {
        TokenKind::KwRef
    };
    [error] => {
        TokenKind::Error
    };
    [ws] => {
        TokenKind::Ws
    };
    [eof] => {
        TokenKind::Eof
    };
    [&&] => {
        TokenKind::And
    };
    [||] => {
        TokenKind::Or
    };
    [==] => {
        TokenKind::Eqq
    };
    [!=] => {
        TokenKind::Neq
    };
    [>=] => {
        TokenKind::Geq
    };
    [<=] => {
        TokenKind::Leq
    };
}

#[allow(clippy::len_without_is_empty)]
impl TokenKind {
    #[inline(always)]
    pub const fn is_trivia(self) -> bool {
        matches!(self, TokenKind::Ws)
    }

    #[inline(always)]
    pub const fn len(self) -> usize {
        match self {
            T![&&] | T![||] | T![==] | T![!=] | T![>=] | T![<=] | T![::] | T![->] => 2,
            _ => 1,
        }
    }

    pub const fn try_as_str(&self) -> Option<&str> {
        let repr = match self {
            T![+] => "+",
            T![-] => "-",
            T![*] => "*",
            T![/] => "/",
            T![%] => "%",
            T![^] => "^",
            T![=] => "=",
            T![.] => ".",
            T![,] => ",",
            T![_] => "_",
            T![!] => "!",
            T![&] => "&",
            T![|] => "|",
            T![?] => "?",
            T![:] => ":",
            T![;] => ";",
            T![#] => "#",
            T![$] => "$",
            T!['"'] => r#"""#,
            // Bracket
            T![<] => "<",
            T![>] => ">",
            T!['['] => "[",
            T![']'] => "]",
            T!['{'] => "{",
            T!['}'] => "}",
            T!['('] => "(",
            T![')'] => ")",
            // Combinations
            T![::] => "::",
            T![->] => "->",
            T![string] => r#""String""#,
            T![char] => "'char'",
            T![line comment] => "// Comment",
            T![block comment] => "/* Comment */",
            T![bool] => "Bool",
            T![int] => "Int",
            T![float] => "Float",
            T![ident] => "Identifier",
            // Keywords
            T![let] => "let",
            T![const] => "const",
            T![if] => "if",
            T![else] => "else",
            T![match] => "match",
            T![while] => "while",
            T![for] => "for",
            T![loop] => "loop",
            T![in] => "in",
            T![extern] => "extern",
            T![fn] => "fn",
            T![return] => "return",
            T![mod] => "mod",
            T![class] => "class",
            T![ref] => "ref",
            // Operators
            T![&&] => "&&",
            T![||] => "||",
            T![==] => "==",
            T![!=] => "!=",
            T![>=] => ">=",
            T![<=] => "<=",
            // Misc =>
            T![error] => "<?>",
            T![ws] => "<Whitespace>",
            T![eof] => "<EOF>",
        };
        Some(repr)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[allow(clippy::write_literal)]
        // required for writing curly braces, which otherwise indicate a format literal
        match self.try_as_str() {
            Some(repr) => write!(f, "{}", repr),
            None => unimplemented!(),
        }
    }
}
