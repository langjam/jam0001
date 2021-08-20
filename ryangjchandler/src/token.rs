#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    pub line: u16,
    pub column: u16,
}

impl Position {
    pub fn new(line: u16, column: u16) -> Self {
        Self { line, column }
    }

    pub fn none() -> Self {
        Self {
            line: 0,
            column: 0
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

impl Token {

    pub fn new(kind: TokenKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn none() -> Self {
        Self {
            kind: TokenKind::None,
            position: Position::none(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    CommentStarter,
    MethodDirective(String),
    FileDirective(String),
    Identifier(String),
    CommentTerminator,
    Asterisk,
    String(String),
    ForwardSlash,
    BackSlash,
    Eof,
    Fn,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    None,
}

pub type TokenList = Vec<Token>;