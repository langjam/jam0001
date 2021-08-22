use std::fmt::{Display, Formatter};
use std::cell::Cell;
use serde::Serialize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
impl Span {
    pub fn from_length(start: usize, length: usize) -> Self {
        Span{start, end: start + length}
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParserError {
    pub span: Span,
    pub error: String,
    pub input: String,
}
impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        //Find before
        let mut line_start = self.span.start;
        while line_start > 0 && self.input[(line_start - 1)..].chars().next().unwrap() != '\n' && self.input[(line_start - 1)..].chars().next().unwrap() != '\r' {
            line_start -= 1;
        }

        //Find after
        let mut line_end = self.span.end;
        while line_end < self.input.len() && self.input[line_end..].chars().next().unwrap() != '\n' && self.input[line_end..].chars().next().unwrap() != '\r' {
            line_end += 1;
        }

        writeln!(f, "Parse error in line:")?;
        writeln!(f, "{}", &self.input[line_start..line_end])?;
        write!(f, "{: <1$}", "", self.span.start - line_start)?;
        write!(f, "{:^<1$}", "", self.span.end - self.span.start)?;
        writeln!(f, "{: <1$}", "", line_end - self.span.end)?;
        writeln!(f, "{}", self.error)?;
        Ok(())
    }
}

pub type ParseResult<T> = Result<T, ParserError>;

pub struct Parser<'a> {
    pub input: &'a str,
    pub current: usize,
    pub current_identifier: Cell<i64>,
}
