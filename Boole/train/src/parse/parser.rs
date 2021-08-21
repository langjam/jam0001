use std::io::repeat;

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
        //Find before
        let mut line_start = self.span.start;
        while line_start > 0 && input[(line_start - 1)..].chars().next().unwrap() != '\n' {
            line_start -= 1;
        }

        //Find after
        let mut line_end = self.span.end;
        while line_end < input.len() && input[line_end..].chars().next().unwrap() != '\n' {
            line_end += 1;
        }

        println!("Parse error in line:");
        println!("{}", &input[line_start..line_end]);
        print!("{: <1$}", "", self.span.start - line_start);
        print!("{:^<1$}", "", self.span.end - self.span.start);
        println!("{: <1$}", "", line_end - self.span.end);
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