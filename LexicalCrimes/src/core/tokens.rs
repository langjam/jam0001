use logos::Logos;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub enum Token {
    /*
        +----------+
        | Keywords |
        +----------+
    */
    
    ///
    /// ```
    /// let name = "regretti"
    /// ```
    /// 
    #[token("let")]
    Variable,

    IfCondition,

    ConditionMet,

    ConditionNotMet,

    ConditionHandler,

    Loop,

    LoopBack,

    ///
    /// ```
    /// main:
    ///     ...
    /// ```
    ///
    #[token("main:")]
    MainFunction,

    ///
    /// ```
    /// funcname:
    ///     ...
    /// ```
    ///
    #[regex("[a-zA-Z_]+:")]
    FunctionStart,

    #[token(":end")]
    FunctionEnd,

    #[token("/*")]
    CommandStart,

    #[regex(r"\*/")]
    CommandEnd,

    #[regex(r"\|(.*)\|")]
    Statement,

    #[token("\n")]
    Newline,

    #[regex(r"[a-zA-Z_]+\((.*)\)\?")]
    LibFunctionNoExecute,

    #[regex(r"[a-zA-Z_]+\((.*)\)")]
    LibFunction,

    #[regex("[a-zA-Z_]+")]
    Keyword,

    #[regex(r"\#(.*)", logos::skip)]
    Comments,

    /*
        +-----------+
        | Datatypes |
        +-----------+
    */
    
    #[regex("[0-9]+")]
    Number,

    Math,

    #[regex("\"(.*)\"")]
    String,

    /*
        +-----------+
        | Operators |
        +-----------+
    */
    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Multiply,
    
    #[token("/")]
    Divide,

    #[token("=")]
    Assign,

    #[token("==")]
    Equals,   

    #[token("!=")]
    NotEquals,

    #[token(">")]
    GreaterThan,
    
    #[token("<")]
    LesserThan,

    #[token(">=")]
    GreaterThanOrEquals,

    #[token("<=")]
    LesserThanOrEquals,

    /*
        +------------+
        | Error/Skip |
        +------------+
    */
    // whitespace
    #[regex(r"[ \t\f]+", logos::skip)]
    // syntatic sugar
    #[regex(r"\+(.*)\+", logos::skip)]    
    Skipped,

    #[error]
    PassLex,
}

use once_cell::sync::Lazy;
use regex::Regex;

// parser grammar
pub static STATEMENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"\|(.*)\|").unwrap());
pub static LOOP_BACK: Lazy<Regex> = Lazy::new(|| Regex::new(r"\|(.*)\^").unwrap());
pub static STRING: Lazy<Regex> = Lazy::new(|| Regex::new("\"(.*)\"").unwrap());
pub static MATH: Lazy<Regex> = Lazy::new(|| Regex::new(r"[+\-*/]").unwrap());
pub static IF_FLOW_RIGHT: Lazy<Regex> = Lazy::new(|| Regex::new("-(.*)>").unwrap());
pub static IF_FLOW_LEFT: Lazy<Regex> = Lazy::new(|| Regex::new("<(.*)-").unwrap());
pub static ELSE_BLOCK: Lazy<Regex> = Lazy::new(|| Regex::new("\\|(.*)else").unwrap());
pub static BLOCK: Lazy<Regex> = Lazy::new(|| Regex::new(r"\+(.*)\+").unwrap());

/// Returns the token type from grammar matches
/// 
/// This is matched based on priority, goal is to make fewer Regex matches
pub fn token_grammar(slice: &str) -> Token {
    if IF_FLOW_RIGHT.is_match(slice) {
        Token::IfCondition
    } else if IF_FLOW_LEFT.is_match(slice) {
        Token::LoopBack
    } else if ELSE_BLOCK.is_match(slice) {
        Token::Statement
    } else if STATEMENT.is_match(slice) {
        Token::Statement
    } else if LOOP_BACK.is_match(slice) {
        use super::builtins::{Function, Builtin};
        Function::loop_back();
        Token::Skipped
    } else if STRING.is_match(slice) {
        Token::String
    } else if BLOCK.is_match(slice) {
        Token::Skipped
    } else {
        // expression capture
        if MATH.captures(slice).is_some() {
            let capture = MATH.captures(slice).unwrap();
            if capture.len() > 0 {
                Token::Math
            } else {
                Token::Skipped
            }
        } else {
            Token::Skipped
        }
    }
}