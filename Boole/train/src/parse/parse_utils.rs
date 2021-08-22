use crate::parse::parser::*;
use std::cell::Cell;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser{input, current: 0, current_identifier: Cell::new(0)}
    }

    pub fn rest(&self) -> &'a str {
        &self.input[self.current..]
    }

    pub fn next_line(&self) -> ParseResult<&'a str> {
        match self.rest().lines().next() {
            None => {
                Err(ParserError {
                    span: Span::from_length(self.current, 0),
                    error: format!("Unexpected end of file."),
                    input: self.input.to_string(),
                })
            }
            Some(line) => {
                Ok(line)
            }
        }
    }

    pub fn expect_word(&mut self) -> ParseResult<String> {
        let line = self.next_line()?;
        let word = line.split(|c: char| !c.is_alphanumeric()).next().unwrap();
        self.current += word.len();
        Ok(String::from(word))
    }

    pub fn expect_number(&mut self) -> ParseResult<isize> {
        let line = self.next_line()?;
        let word = line.split_whitespace().next().unwrap();

        let num = word.parse::<isize>();
        if let Ok(num) = num {
            self.current += word.len();
            Ok(num)
        } else {
            Err(ParserError {
                span: Span::from_length(self.current, word.len()),
                error: format!("Expected a number here."),
                input: self.input.to_string(),
            })
        }
    }
    pub fn expect_exact_text(&mut self, exact: &str) -> ParseResult<()> {
        let line = self.next_line()?;
        if line.starts_with(exact) {
            self.current += exact.len();
            Ok(())
        } else {
            Err(ParserError {
                span: Span::from_length(self.current, 1),
                error: format!("Expected the exact text '{}' here.", exact),
                input: self.input.to_string(),
            })
        }
    }

    pub fn expect_exact_line(&mut self, exact: &str) -> ParseResult<()> {
        let line = self.next_line()?;
        if line == exact {
            self.current += line.len();
            self.skip_line()?;
            Ok(())
        } else {
            Err(ParserError {
                span: Span::from_length(self.current, line.len()),
                error: format!("Expected the exact line '{}' here.", exact),
                input: self.input.to_string(),
            })
        }
    }

    pub fn skip_line(&mut self) -> ParseResult<()> {
        if self.rest().chars().next().unwrap() == '\r' {
            self.current += 1;
        }
        if self.rest().chars().next().unwrap() == '\n' {
            self.current += 1;
        }
        Ok(())
    }
}