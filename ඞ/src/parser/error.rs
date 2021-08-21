use super::*;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        found: TokenKind,
        expected: Vec<TokenKind>,
        position: Span,
    },
    InvalidExpressionStatement {
        position: Span,
    },
}

impl From<&ParseError> for String {
    fn from(error: &ParseError) -> Self {
        match error {
            ParseError::UnexpectedToken {
                found,
                expected,
                position,
            } => {
                format!(
                    "Unexpected token at {}-{}: found '{}', but expected {}",
                    position.start,
                    position.end,
                    found,
                    token_list_to_string(expected)
                )
            }
            ParseError::InvalidExpressionStatement { position } => format!(
                "Expression is invalid as statement at {}-{}",
                position.start, position.end,
            ),
        }
    }
}

impl From<ParseError> for String {
    fn from(error: ParseError) -> Self {
        String::from(&error)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from(self))
    }
}

impl std::error::Error for ParseError {}

fn token_list_to_string(tokens: &[TokenKind]) -> String {
    let res: Vec<String> = tokens.iter().map(|token| format!("'{}'", token)).collect();
    let mut res = res.join(", ");
    if let Some(pos) = res.rfind(", ") {
        res.replace_range(pos..=pos + 1, " or ");
        res.replace_range(0..0, "one of ");
    }
    res
}
