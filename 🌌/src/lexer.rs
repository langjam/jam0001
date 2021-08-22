use core::fmt;
use std::{iter::Peekable, str::Chars};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    Assign,
    Semicolon,
    Dot,
    Comma,
    Integer(String),
    Float(String),
    String(String),
    Identifier(String, Vec<String>),
    Func,
    Let,
    Return,
    Break,
    Continue,
    Export,
    Import,
    If,
    Else,
    While,
    Struct,
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexerError {
    message: String,
    line: usize,
    column: usize,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} in line {}, col {}",
            self.message, self.line, self.column
        )
    }
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            chars: code.chars().peekable(),
            line: 1,
            column: 0,
        }
    }

    pub fn into_tokens(mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let next = self.next()?;
            match next.token_type {
                TokenType::Eof => {
                    tokens.push(next);
                    break;
                }
                _ => {
                    tokens.push(next);
                }
            }
        }
        Ok(tokens)
    }

    fn error(&self, message: String) -> LexerError {
        LexerError {
            message,
            line: self.line,
            column: self.column,
        }
    }

    fn peek(&mut self) -> char {
        *(self.chars.peek().unwrap_or(&char::default()))
    }

    fn advance(&mut self) -> char {
        let c = self.chars.next().unwrap_or_default();
        self.column += 1;

        if c == '\n' {
            self.line += 1;
            self.column = 0;
        }

        c
    }

    fn consume(&mut self, c: char) -> Result<(), LexerError> {
        if self.peek() == c {
            self.advance();
            Ok(())
        } else {
            let found = self.peek();
            if found == char::default() {
                Err(self.error(format!("Expected '{}', found end of stream", c)))
            } else {
                Err(self.error(format!("Expected '{}', found '{}'", c, found)))
            }
        }
    }

    fn emit(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            line: self.line,
            column: self.column,
        }
    }

    pub fn next(&mut self) -> Result<Token, LexerError> {
        while self.peek().is_whitespace() {
            self.advance();
        }

        if self.peek() == '{' {
            self.advance();
            Ok(self.emit(TokenType::LeftBrace))
        } else if self.peek() == '}' {
            self.advance();
            Ok(self.emit(TokenType::RightBrace))
        } else if self.peek() == '(' {
            self.advance();
            Ok(self.emit(TokenType::LeftParen))
        } else if self.peek() == ')' {
            self.advance();
            Ok(self.emit(TokenType::RightParen))
        } else if self.peek() == '[' {
            self.advance();
            Ok(self.emit(TokenType::LeftSquare))
        } else if self.peek() == ']' {
            self.advance();
            Ok(self.emit(TokenType::RightSquare))
        } else if self.peek() == ';' {
            self.advance();
            Ok(self.emit(TokenType::Semicolon))
        } else if self.peek() == ':' {
            self.advance();
            self.consume('=')?;
            Ok(self.emit(TokenType::Assign))
        } else if self.peek() == '.' {
            self.advance();
            Ok(self.emit(TokenType::Dot))
        } else if self.peek() == ',' {
            self.advance();
            Ok(self.emit(TokenType::Comma))
        } else if self.peek() == '+' {
            self.advance();
            Ok(self.emit(TokenType::Plus))
        } else if self.peek() == '-' {
            self.advance();
            Ok(self.emit(TokenType::Minus))
        } else if self.peek() == '*' {
            self.advance();
            Ok(self.emit(TokenType::Star))
        } else if self.peek() == '/' {
            self.advance();
            Ok(self.emit(TokenType::Slash))
        } else if self.peek() == '%' {
            self.advance();
            Ok(self.emit(TokenType::Modulo))
        } else if self.peek() == '&' {
            self.advance();
            self.consume('&')?;
            Ok(self.emit(TokenType::And))
        } else if self.peek() == '|' {
            self.advance();
            self.consume('|')?;
            Ok(self.emit(TokenType::Or))
        } else if self.peek() == '!' {
            self.advance();
            if self.peek() == '=' {
                self.advance();
                Ok(self.emit(TokenType::NotEqual))
            } else {
                Ok(self.emit(TokenType::Not))
            }
        } else if self.peek() == '=' {
            self.advance();
            if self.peek() == '=' {
                self.advance();
                Ok(self.emit(TokenType::Equal))
            } else {
                Err(self.error("Invalid '=' token. Did you mean ':='?".to_owned()))
            }
        } else if self.peek() == '<' {
            self.advance();
            if self.peek() == '=' {
                self.advance();
                Ok(self.emit(TokenType::LessThanOrEqual))
            } else {
                Ok(self.emit(TokenType::LessThan))
            }
        } else if self.peek() == '>' {
            self.advance();
            if self.peek() == '=' {
                self.advance();
                Ok(self.emit(TokenType::GreaterThanOrEqual))
            } else {
                Ok(self.emit(TokenType::GreaterThan))
            }
        } else if self.peek() == '"' {
            Ok(self.consume_string()?)
        } else if self.peek().is_digit(10) {
            Ok(self.consume_number()?)
        } else if self.peek() == '`' {
            Ok(self.consume_backtick_identifier())
        } else if Self::is_naked_identifier_char(self.peek()) {
            Ok(self.consume_naked_identifier())
        } else if self.peek() == char::default() {
            Ok(self.emit(TokenType::Eof))
        } else {
            let peek = self.peek();
            Err(self.error(format!("Unexpected {}", peek)))
        }
    }

    fn consume_number(&mut self) -> Result<Token, LexerError> {
        let mut value = String::new();
        while self.peek().is_digit(10) {
            value.push(self.advance());
        }

        if self.peek() == '.' {
            self.advance();
            if !self.peek().is_digit(10) {
                return Err(self.error("Expected digit".to_owned()));
            }
            value.push('.');
            while self.peek().is_digit(10) {
                value.push(self.advance());
            }
            Ok(self.emit(TokenType::Float(value)))
        } else {
            Ok(self.emit(TokenType::Integer(value)))
        }
    }

    fn consume_string(&mut self) -> Result<Token, LexerError> {
        let mut value = String::new();
        self.consume('"')?;
        while self.peek() != '"' && self.peek() != char::default() {
            if self.peek() == '\\' {
                self.advance();
                match self.peek() {
                    'n' => {
                        value.push('\n');
                        self.advance();
                    }
                    't' => {
                        value.push('\t');
                        self.advance();
                    }
                    '"' | '\\' => {
                        value.push(self.advance());
                    }
                    c => return Err(self.error(format!("Unknown escape '\\{}'", c))),
                };
            } else {
                value.push(self.advance());
            }
        }
        self.consume('"')?;
        Ok(self.emit(TokenType::String(value)))
    }

    fn is_operator_char(c: char) -> bool {
        "{}()[]:;.,+-*/%&|<>=!".contains(c)
    }

    fn is_naked_identifier_char(c: char) -> bool {
        c != char::default() && !Self::is_operator_char(c)
    }

    fn keyword(&self, value: &str) -> Option<Token> {
        match value.trim_end() {
            "return" => Some(self.emit(TokenType::Return)),
            "let" => Some(self.emit(TokenType::Let)),
            "func" => Some(self.emit(TokenType::Func)),
            "if" => Some(self.emit(TokenType::If)),
            "else" => Some(self.emit(TokenType::Else)),
            "while" => Some(self.emit(TokenType::While)),
            "break" => Some(self.emit(TokenType::Break)),
            "continue" => Some(self.emit(TokenType::Continue)),
            "export" => Some(self.emit(TokenType::Export)),
            "import" => Some(self.emit(TokenType::Import)),
            "struct" => Some(self.emit(TokenType::Struct)),
            _ => None,
        }
    }

    fn consume_naked_identifier(&mut self) -> Token {
        let mut value = String::new();
        let mut args = Vec::new();

        while Self::is_naked_identifier_char(self.peek()) {
            if self.peek().is_whitespace() {
                if let Some(keyword_token) = self.keyword(&value) {
                    return keyword_token;
                }
            } else if self.peek() == '`' {
                let mut arg = String::new();

                value.push(self.advance());
                while self.peek() != '`' {
                    arg.push(self.advance());
                }

                value.push_str(&arg);
                args.push(arg);
            }

            value.push(self.advance())
        }

        if let Some(keyword_token) = self.keyword(value.trim_end()) {
            keyword_token
        } else {
            self.emit(TokenType::Identifier(value.trim_end().to_string(), args))
        }
    }

    fn consume_backtick_identifier(&mut self) -> Token {
        let mut value = String::new();
        self.advance();
        while self.peek() != '`' {
            value.push(self.advance())
        }
        self.advance();
        self.emit(TokenType::Identifier(value, vec![]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(code: &str) -> Result<Vec<TokenType>, LexerError> {
        Lexer::new(code)
            .into_tokens()
            .map(|tokens| tokens.into_iter().map(|t| t.token_type).collect())
    }

    #[test]
    fn a_whole_program_can_be_one_identifier() {
        let program = "why did the chicken cross the road? to get to the 0ther side";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Identifier(program.to_owned(), Vec::new()),
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn naked_identifier_includes_whitespace_after_it() {
        let program = "Hello world := 5;";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Identifier("Hello world".to_owned(), Vec::new()),
                TokenType::Assign,
                TokenType::Integer("5".to_owned()),
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn marked_identifier_can_contain_any_characters() {
        let program = "`Hello, world! := 5 - 2 < 3` := 5;";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Identifier("Hello, world! := 5 - 2 < 3".to_owned(), Vec::new()),
                TokenType::Assign,
                TokenType::Integer("5".to_owned()),
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn keyword_at_the_beginning_of_naked_identifier_is_a_keyword() {
        let program = "return hello world;";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Return,
                TokenType::Identifier("hello world".to_owned(), Vec::new()),
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn keyword_within_identifier_is_part_of_identifier() {
        let program = "hello return world;";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Identifier("hello return world".to_owned(), Vec::new()),
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn keyword_at_the_beginning_of_marked_identifier_is_part_of_identifier() {
        let program = "`return hello world!`;";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Identifier("return hello world!".to_owned(), Vec::new()),
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn naked_identifier_can_contain_embedded_marked_identifiers_as_parameters() {
        let program = "func contains substring checks if `haystack` contains `needle` { }";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Func,
                TokenType::Identifier(
                    "contains substring checks if `haystack` contains `needle`".to_owned(),
                    vec!["haystack".to_owned(), "needle".to_owned()]
                ),
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn operators_break_up_naked_identifiers() {
        let program = "let Hello, world := 42;";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Let,
                TokenType::Identifier("Hello".to_owned(), Vec::new()),
                TokenType::Comma,
                TokenType::Identifier("world".to_owned(), Vec::new()),
                TokenType::Assign,
                TokenType::Integer("42".to_owned()),
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn operators_do_not_break_up_marked_identifiers() {
        let program = "let `Hello, world!` := 42;";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Let,
                TokenType::Identifier("Hello, world!".to_owned(), Vec::new()),
                TokenType::Assign,
                TokenType::Integer("42".to_owned()),
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn string_without_escape_characters() {
        let program = "print with new line(\"Hello, world!\");";

        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::Identifier("print with new line".to_owned(), Vec::new()),
                TokenType::LeftParen,
                TokenType::String("Hello, world!".to_owned()),
                TokenType::RightParen,
                TokenType::Semicolon,
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn string_with_escape_characters() {
        let program = "\"Hello,\\nworld\\t!\\\\\\\"\"";
        assert_eq!(
            lex(program),
            Ok(vec![
                TokenType::String("Hello,\nworld\t!\\\"".to_owned()),
                TokenType::Eof
            ])
        );
    }

    #[test]
    fn string_with_invalid_escape_characters_causes_error() {
        let program = "\"Invalid \\escape\"";
        assert_eq!(
            lex(program),
            Err(LexerError {
                message: "Unknown escape '\\e'".to_owned(),
                line: 1,
                column: 10,
            })
        );
    }

    #[test]
    fn unterminated_string() {
        let program = "\"Hello, world!";
        assert_eq!(
            lex(program),
            Err(LexerError {
                message: "Expected '\"', found end of stream".to_owned(),
                line: 1,
                column: 14,
            })
        );
    }

    #[test]
    fn malformed_operators_return_errors() {
        let program = "let x : 2;";

        assert_eq!(
            lex(program),
            Err(LexerError {
                message: "Expected '=', found ' '".to_owned(),
                line: 1,
                column: 7
            })
        );
    }

    #[test]
    fn lexer_reports_location_of_error() {
        let program = "let x := 2;\nlet y := 3;\n\nlet z := 1 | 2;\n\nlet w := 7;";

        assert_eq!(
            lex(program),
            Err(LexerError {
                message: "Expected '|', found ' '".to_owned(),
                line: 4,
                column: 12
            })
        );
    }
}
