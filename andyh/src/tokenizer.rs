use std::{collections::HashMap, iter::Peekable, str::Chars};

pub type Tokens = Vec<Token>;
pub type Variables = HashMap<String, f64>;

#[derive(Debug)]
pub enum Token {
    Comment(String),
    Number(f64),
    Add,
    Subtract,
    Multiply,
    Divide,
    Sin(f64),
    SinSym(String),
    Cos(f64),
    CosSym(String),
    Tan(f64),
    TanSym(String),
    Floor,
    Ceil,
    Rounded,
    Mod(i64),
    Assignment,
    Symbol(String),
    EndOfExpression,
    Newline,
}

fn ignore_any_whitespace(chars: &mut Peekable<Chars>) {
    loop {
        match chars.peek() {
            Some('\n') => break,
            Some(ch) if ch.is_ascii_whitespace() => chars.next(),
            _ => break,
        };
    }
}

fn ignore_until_end_of_line(chars: &mut Peekable<Chars>) {
    loop {
        match chars.next() {
            Some('\n') | None => break,
            _ => continue,
        }
    }
}

fn is_boundary(ch: &char) -> bool {
    if ch.is_ascii_whitespace() {
        return true;
    }
    matches!(ch, '+' | '-' | '*' | '/' | '\n' | '[' | ']' | ':')
}

fn parse_float(chars: &mut Peekable<Chars>) -> Option<f64> {
    let mut string = String::new();
    if let Some(ch) = chars.peek() {
        if !ch.is_numeric() {
            return None;
        }
    }
    loop {
        string.push(chars.next().expect("numeric"));
        match chars.peek() {
            Some(ch) if ch.is_numeric() => continue,
            Some(ch) if is_boundary(ch) => break,
            None => break,
            Some('.') => {
                loop {
                    string.push(chars.next().expect("numeric"));
                    match chars.peek() {
                        Some(ch) if ch.is_numeric() => continue,
                        Some(ch) if is_boundary(ch) => break,
                        None => break,
                        _ => panic!("illegal token"),
                    }
                }
                break;
            }
            _ => panic!("illegal token"),
        }
    }
    Some(string.parse().expect("f64"))
}

fn parse_int(chars: &mut Peekable<Chars>) -> Option<i64> {
    let mut string = String::new();
    if let Some(ch) = chars.peek() {
        if !ch.is_numeric() {
            return None;
        }
    }
    loop {
        string.push(chars.next().expect("numeric"));
        match chars.peek() {
            Some(ch) if ch.is_numeric() => continue,
            Some(ch) if is_boundary(ch) => break,
            None => break,
            _ => panic!("illegal token"),
        }
    }
    Some(string.parse().expect("i64"))
}

fn parse_symbol(chars: &mut Peekable<Chars>) -> Option<String> {
    let mut symbol = String::new();
    if !matches!(chars.peek(), Some(ch) if ch.is_ascii_alphabetic()) {
        return None;
    }
    loop {
        symbol.push(chars.next().expect("char"));
        match chars.peek() {
            Some('_') => continue,
            Some(ch) if ch.is_ascii_alphanumeric() => continue,
            Some(ch) if is_boundary(ch) => break,
            None => break,
            _ => panic!("unexpected char"),
        }
    }
    Some(symbol.to_ascii_lowercase())
}

fn parse_float_token(chars: &mut Peekable<Chars>) -> Option<Token> {
    parse_float(chars).map(Token::Number)
}

fn parse_operator_token(chars: &mut Peekable<Chars>) -> Option<Token> {
    let token = match chars.peek() {
        Some('+') => Some(Token::Add),
        Some('-') => Some(Token::Subtract),
        Some('*') => Some(Token::Multiply),
        Some('/') => Some(Token::Divide),
        Some(':') => Some(Token::Assignment),
        _ => return None,
    };
    if token.is_some() {
        chars.next().expect("symbol");
    }
    if matches!(token, Some(Token::Divide)) && matches!(chars.peek(), Some('/')) {
        ignore_until_end_of_line(chars);
        return Some(Token::EndOfExpression);
    }
    token
}

fn parse_newline_token(chars: &mut Peekable<Chars>) -> Option<Token> {
    if matches!(chars.peek(), Some('\n')) {
        chars.next().expect("newline");
        Some(Token::EndOfExpression)
    } else {
        None
    }
}

fn parse_comment_token(chars: &mut Peekable<Chars>, vars: &mut Variables) -> Option<Token> {
    if matches!(chars.peek(), Some('/')) {
        chars.next().expect("slash");
        if !matches!(chars.peek(), Some('/')) {
            panic!("illegal token!");
        }
        chars.next().expect("slash");
    } else {
        return None;
    }
    ignore_any_whitespace(chars);
    let mut string = String::new();
    while let Some(ch) = chars.next() {
        match ch {
            '\n' => break,
            '[' => {
                ignore_any_whitespace(chars);
                let key = parse_symbol(chars).expect("symbol");
                ignore_any_whitespace(chars);
                if !matches!(chars.next(), Some(':')) {
                    panic!("expected seperator");
                }
                ignore_any_whitespace(chars);
                let value = parse_float(chars).expect("float");
                ignore_any_whitespace(chars);
                if !matches!(chars.next(), Some(']')) {
                    panic!("illegal token");
                }
                string.push_str(&format!("[ {}: {} ]", key, value.to_string()));
                if vars.contains_key(&string) {
                    panic!("variable already defined");
                }
                vars.insert(key, value);
            }
            _ => string.push(ch),
        }
    }
    Some(Token::Comment(string))
}

fn parse_symbol_token(chars: &mut Peekable<Chars>) -> Option<Token> {
    match parse_symbol(chars) {
        Some(string) => match string.as_str() {
            "sin" => {
                ignore_any_whitespace(chars);
                if let Some(value) = parse_float(chars) {
                    return Some(Token::Sin(value));
                }
                if let Some(string) = parse_symbol(chars) {
                    return Some(Token::SinSym(string));
                }
                panic!("expected value");
            }
            "cos" => {
                ignore_any_whitespace(chars);
                if let Some(value) = parse_float(chars) {
                    return Some(Token::Cos(value));
                }
                if let Some(string) = parse_symbol(chars) {
                    return Some(Token::CosSym(string));
                }
                panic!("expected value");
            }
            "tan" => {
                ignore_any_whitespace(chars);
                if let Some(value) = parse_float(chars) {
                    return Some(Token::Tan(value));
                }
                if let Some(string) = parse_symbol(chars) {
                    return Some(Token::TanSym(string));
                }
                panic!("expected value");
            }
            "mod" => {
                ignore_any_whitespace(chars);
                match parse_int(chars) {
                    Some(value) => Some(Token::Mod(value)),
                    None => panic!("expected value"),
                }
            }
            "floor" => Some(Token::Floor),
            "ceil" => Some(Token::Ceil),
            "rounded" => Some(Token::Rounded),
            _ => Some(Token::Symbol(string)),
        },
        None => None,
    }
}

pub fn tokenize(input: &str) -> (Tokens, Variables) {
    let mut tokens = Tokens::new();
    let mut vars = Variables::new();
    let mut chars = input.chars().peekable();
    while chars.peek().is_some() {
        ignore_any_whitespace(&mut chars);
        if matches!(
            tokens.last(),
            Some(Token::Comment(_) | Token::EndOfExpression | Token::Newline) | None
        ) {
            if let Some(token) = parse_comment_token(&mut chars, &mut vars) {
                tokens.push(token);
                continue;
            }
        }
        if let Some(token) = parse_float_token(&mut chars) {
            tokens.push(token);
            continue;
        }
        if let Some(token) = parse_operator_token(&mut chars) {
            tokens.push(token);
            continue;
        }
        if let Some(token) = parse_newline_token(&mut chars) {
            if !matches!(tokens.last(), Some(Token::EndOfExpression) | None) {
                tokens.push(token);
            }
            if matches!(tokens.last(), Some(Token::EndOfExpression)) {
                tokens.push(Token::Newline);
            }
            continue;
        }
        if let Some(token) = parse_symbol_token(&mut chars) {
            tokens.push(token);
            continue;
        }
        panic!("illegal token");
    }
    if !matches!(
        tokens.last(),
        Some(Token::EndOfExpression | Token::Comment(_)) | None
    ) {
        tokens.push(Token::EndOfExpression);
    }
    (tokens, vars)
}
