#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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
}
impl ParserError {
    pub fn print(&self, input: &str) {
        println!("Parse error!");
        println!("At [{}-{}]: '{}'", self.span.start, self.span.end, &input[self.span.start..self.span.end]);
        println!("{}", self.error);
    }
}

pub type ParseResult<'a, T> = Result<T, ParserError>;

pub struct Parser<'a> {
    pub input: &'a str,
    pub current: usize,
}



#[cfg(test)]
mod tests {
    use super::*;


}