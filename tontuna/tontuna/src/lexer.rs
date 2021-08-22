use logos::Logos;

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Copy, Clone, Logos)]
pub(crate) enum TokenKind {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("return")]
    Return,
    #[token("struct")]
    Struct,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex("\"([^\\n\"\\\\]|\\\\.)*\"")]
    Str,
    #[token("nil")]
    Nil,
    #[token("self")]
    SelfKw,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("=")]
    Equals,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftCurly,
    #[token("}")]
    RightCurly,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Name,
    #[regex("[0-9][a-zA-Z0-9_]*")]
    Number,
    #[token("#")]
    CommentMarker,
    CodeMarker,
    #[regex(" +")]
    Space,
    #[regex(r"\r?\n")]
    Newline,
    #[error]
    Error,
    CommentText,
}

impl TokenKind {
    pub(crate) fn to_str(self) -> &'static str {
        match self {
            TokenKind::Fn => "`fn`",
            TokenKind::Let => "`let`",
            TokenKind::While => "`while`",
            TokenKind::If => "`if`",
            TokenKind::Else => "`else`",
            TokenKind::For => "`for`",
            TokenKind::In => "`in`",
            TokenKind::Return => "`return`",
            TokenKind::Struct => "`struct`",
            TokenKind::True => "`true`",
            TokenKind::False => "`false`",
            TokenKind::Str => "string literal",
            TokenKind::Nil => "`nil`",
            TokenKind::SelfKw => "`self`",
            TokenKind::Dot => "`.`",
            TokenKind::Colon => "`:`",
            TokenKind::Equals => "`=`",
            TokenKind::And => "`&&`",
            TokenKind::Or => "`||`",
            TokenKind::Plus => "`+`",
            TokenKind::Minus => "`-`",
            TokenKind::Star => "`*`",
            TokenKind::Slash => "`/`",
            TokenKind::Less => "`<`",
            TokenKind::LessEq => "`<=`",
            TokenKind::Greater => "`>`",
            TokenKind::GreaterEq => "`>=`",
            TokenKind::EqEq => "`==`",
            TokenKind::NotEq => "`!=`",
            TokenKind::LeftParen => "`(`",
            TokenKind::RightParen => "`)`",
            TokenKind::LeftCurly => "`{`",
            TokenKind::RightCurly => "`}`",
            TokenKind::Comma => "`,`",
            TokenKind::Semicolon => "`;`",
            TokenKind::Name => "identifier",
            TokenKind::Number => "number",
            TokenKind::CommentMarker => "`#`",
            TokenKind::CodeMarker => "`>`",
            TokenKind::Space => "whitespace",
            TokenKind::Newline => "whitespace",
            TokenKind::Error => "bad token",
            TokenKind::CommentText => "comment text",
        }
    }
}

pub(crate) fn next_token(source: &str) -> Option<(TokenKind, usize)> {
    let mut lexer = TokenKind::lexer(source);
    let token = lexer.next()?;
    let len = source.len() - lexer.remainder().len();
    Some((token, len))
}
