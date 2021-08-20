use bitflags::bitflags;
use pyo3::prelude::*;
use pyo3::{wrap_pyfunction, PyObjectProtocol};

#[derive(PartialEq, Debug, Clone, Copy)]
enum Tokens {
    EOF,

    Function,
    Class,
    Struct,
    TypeName,
    Operator,

    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,

    Dot,
    Comma,

    Assignment,
    Semicolon,
    Colon,
    Tag,
    Reference,
    Question,
    At,
    Percent,
    Bang,
    BackSlash,

    Arrow,
    Equal,

    Space,
    Tab,
    Newline,

    SingleQuote,
    DoubleQuote,
    Identifier,
    NumericLiteral,
    StringLiteral,

    LoopExit,
    Return,
}

impl Tokens {
    fn from_i32(value: i32) -> Tokens {
        match value {
            0 => Tokens::EOF,

            1 => Tokens::Function,
            2 => Tokens::Class,
            3 => Tokens::Struct,
            4 => Tokens::TypeName,
            5 => Tokens::Operator,

            6 => Tokens::LeftBrace,
            7 => Tokens::RightBrace,
            8 => Tokens::LeftBracket,
            9 => Tokens::RightBracket,
            10 => Tokens::LeftParen,
            11 => Tokens::RightParen,

            12 => Tokens::Dot,
            13 => Tokens::Comma,

            14 => Tokens::Assignment,
            15 => Tokens::Semicolon,
            16 => Tokens::Colon,
            17 => Tokens::Tag,
            18 => Tokens::Reference,
            19 => Tokens::Question,
            20 => Tokens::At,
            21 => Tokens::Percent,
            22 => Tokens::Bang,
            23 => Tokens::BackSlash,

            24 => Tokens::Arrow,
            25 => Tokens::Equal,

            26 => Tokens::Space,
            27 => Tokens::Tab,
            28 => Tokens::Newline,

            29 => Tokens::SingleQuote,
            30 => Tokens::DoubleQuote,
            31 => Tokens::Identifier,
            32 => Tokens::NumericLiteral,
            33 => Tokens::StringLiteral,

            34 => Tokens::LoopExit,
            35 => Tokens::Return,

            _ => panic!("Unknown value: {}", value),
        }
    }
}

#[pyfunction]
fn token_num_to_name(num: i32) -> String {
    format!("{:?}", Tokens::from_i32(num))
}

#[pyclass]
#[derive(PartialEq, Debug)]
struct Token {
    part: String,
    token: Tokens,
}

#[pyproto]
impl PyObjectProtocol for Token {
    fn __str__(&self) -> PyResult<String> {
        let part = match self.part.as_str() {
            "\n" => "\\n",
            "\t" => "\\t",
            _ => &self.part,
        };

        Ok(format!(
            "Token(\"{}\", {:?}: {})",
            part, self.token, self.token as i32
        ))
    }

    fn __repr__(&self) -> PyResult<String> {
        Ok(format!(
            "Token(\"{}\", {:?}: {})",
            self.part, self.token, self.token as i32
        ))
    }
}

#[pymethods]
impl Token {
    #[new]
    fn new(part: String, token: i32) -> Self {
        Token {
            part,
            token: Tokens::from_i32(token),
        }
    }
    #[getter]
    fn part(&self) -> PyResult<String> {
        Ok(self.part.clone())
    }
    #[getter]
    fn token(&self) -> PyResult<i32> {
        Ok(self.token as i32)
    }

    #[setter]
    fn set_part(&mut self, value: String) -> PyResult<()> {
        self.part = value;
        Ok(())
    }

    #[setter]
    fn set_token(&mut self, value: i32) -> PyResult<()> {
        self.token = Tokens::from_i32(value);
        Ok(())
    }
}

#[pyfunction]
fn is_char_symbol(ch: char) -> bool {
    match ch {
        '[' | ']' | '{' | '}' | '(' | ')' | '.' | ',' | ':' | ';' | '=' | '\'' | '\"' | '\\' => {
            true
        }
        _ => false,
    }
}

#[pyfunction]
fn is_char_operator(ch: char) -> bool {
    match ch {
        '+' | '-' | '*' | '/' | '^' | '>' | '<' => true,
        _ => false,
    }
}

#[pyfunction]
fn is_char_whitespace(ch: char) -> bool {
    match ch {
        '\t' | ' ' | '\n' => true,
        _ => false,
    }
}

#[pyfunction]
fn is_char_numeric(ch: char) -> bool {
    return ch.is_digit(10);
}

#[pyfunction]
fn is_single_quote(ch: char) -> bool {
    return ch == '\'';
}

#[pyfunction]
fn is_double_quote(ch: char) -> bool {
    return ch == '\"';
}

#[pyfunction]
fn ends_token(cur: char, next: char) -> bool {
    if is_char_whitespace(next) {
        return true;
    }
    if is_char_symbol(cur) {
        return true;
    }
    if is_char_symbol(next) {
        return true;
    }
    if is_char_operator(cur) {
        return true;
    }
    if is_char_operator(next) {
        return true;
    }
    if is_char_whitespace(cur) {
        return false;
    }
    return false;
}

#[pyfunction]
fn is_part_numeric(part: &str) -> bool {
    for c in part.chars() {
        if is_char_numeric(c) {
            return true;
        }
    }
    return false;
}

#[pyfunction]
fn tokenize(part: &str) -> Token {
    let mut token = match part {
        "fn" | "fun" | "func" | "function" => Tokens::Function,
        "class" | "cls" => Tokens::Class,
        "struct" => Tokens::Struct,
        "int" | "float" | "bool" | "double" | "long" | "str" | "string" | "char" | "short"
        | "void" => Tokens::TypeName,
        "+" | "-" | "*" | "/" | "^" | ">" | "<" => Tokens::Operator,
        "continue" | "break" => Tokens::LoopExit,
        "return" => Tokens::Return,

        "{" => Tokens::LeftBrace,
        "}" => Tokens::RightBrace,
        "[" => Tokens::LeftBracket,
        "]" => Tokens::RightBracket,
        "(" => Tokens::LeftParen,
        ")" => Tokens::RightParen,

        "." => Tokens::Dot,
        "," => Tokens::Comma,

        "=" => Tokens::Assignment,
        ";" => Tokens::Semicolon,
        ":" => Tokens::Colon,
        "#" => Tokens::Tag,
        "&" => Tokens::Reference,
        "?" => Tokens::Question,
        "@" => Tokens::At,
        "%" => Tokens::Percent,
        "!" => Tokens::Bang,
        "\\" => Tokens::BackSlash,

        "->" => Tokens::Arrow,
        "==" => Tokens::Equal,

        " " => Tokens::Space,
        "\t" => Tokens::Tab,
        "\n" => Tokens::Newline,

        "\'" => Tokens::SingleQuote,
        "\"" => Tokens::DoubleQuote,
        _ => Tokens::Identifier,
    };

    let mut part = String::from(part);
    if token == Tokens::Identifier {
        if is_part_numeric(&part) {
            token = Tokens::NumericLiteral;
        }

        if part.ends_with("\"") {
            token = Tokens::StringLiteral;
            part.pop();
        }

        if part.ends_with("\'") {
            token = Tokens::StringLiteral;
            part.pop();
        }
    }

    return Token { part, token };
}

#[pyclass]
struct Lexer {
    index: usize,
    length: usize,
    chars: Vec<char>,
    curr_char: char,
    next_char: char,
    eof: bool,
    settings: u32,
}

bitflags! {
    struct Settings: u32 {
        const PARSE_STRING = 0b00000001;
        const ALL = Self::PARSE_STRING.bits;
    }
}

#[pymethods]
impl Lexer {
    /// The EOF code for python to access
    #[classattr]
    const EOF: i32 = Tokens::EOF as i32;

    #[new]
    fn new(input: String, settings: u32) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let length = chars.len();

        Lexer {
            index: 0,
            chars,
            length,
            eof: false,
            curr_char: ' ',
            next_char: ' ',
            settings,
        }
    }

    fn char_forward(&mut self) {
        self.index += 1;
        self.curr_char = self.chars[self.index];
    }

    fn skip_over_char_set(&mut self, ch: char) -> String {
        let mut string: String = String::new();
        self.char_forward();
        while !(self.curr_char == ch) {
            string.push(self.curr_char);
            self.char_forward();
        }

        // Add something at the end to identify it
        string = string + &ch.to_string();
        self.char_forward();
        return string;
    }

    fn next(&mut self) -> Option<Token> {
        let mut buffer = String::new();
        loop {
            if self.eof {
                return Some(Token {
                    part: "".to_string(),
                    token: Tokens::EOF,
                });
            }

            if self.index + 1 >= self.length {
                self.eof = true;

                self.curr_char = self.chars[self.index];
                buffer.push(self.curr_char);
                return Some(tokenize(&buffer));
            }

            self.curr_char = self.chars[self.index];
            self.next_char = self.chars[self.index + 1];

            if (self.settings & Settings::PARSE_STRING.bits) == Settings::PARSE_STRING.bits {
                if self.curr_char == '"' {
                    let skipped_over = self.skip_over_char_set('"');
                    return Some(tokenize(&skipped_over));
                }

                if self.curr_char == '\'' {
                    let skipped_over = self.skip_over_char_set('\'');
                    return Some(tokenize(&skipped_over));
                }
            }

            if !is_char_whitespace(self.curr_char) {
                buffer.push(self.curr_char);
                if ends_token(self.curr_char, self.next_char) {
                    self.index += 1;
                    return Some(tokenize(&buffer));
                }
            }
            self.index += 1;
        }
    }
}

#[pymodule]
fn jai(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(is_char_symbol, m)?)?;
    m.add_function(wrap_pyfunction!(is_char_operator, m)?)?;
    m.add_function(wrap_pyfunction!(is_char_whitespace, m)?)?;
    m.add_function(wrap_pyfunction!(is_char_numeric, m)?)?;
    m.add_function(wrap_pyfunction!(is_single_quote, m)?)?;
    m.add_function(wrap_pyfunction!(is_double_quote, m)?)?;
    m.add_function(wrap_pyfunction!(ends_token, m)?)?;
    m.add_function(wrap_pyfunction!(is_part_numeric, m)?)?;

    m.add_function(wrap_pyfunction!(token_num_to_name, m)?)?;

    m.add_class::<Lexer>()?;
    m.add_class::<Token>()?;

    Ok(())
}
