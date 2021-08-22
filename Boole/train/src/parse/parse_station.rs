use crate::parse::parser::*;
use crate::ast::*;
use crate::operations::Operation;
use std::collections::HashMap;
use strum::*;

impl<'a> Parser<'a> {
    pub fn parse_operation(&mut self) -> ParseResult<Operation> {
        lazy_static! {
            static ref OPERATION_LOOKUP: HashMap<&'static str, Operation> = {
                let mut map = HashMap::new();
                for operation in Operation::iter() {
                    map.insert(operation.name(), operation);
                }
                map
            };
        }
        let op_text = self.rest().lines().next().unwrap();
        let op = match OPERATION_LOOKUP.get(&op_text) {
            Some(op) => op,
            None => {
                return Err(ParserError {
                    span: Span::from_length(self.current, op_text.len()),
                    error: format!("Operation not found."),
                    input: self.input.to_string(),
                })
            }
        };
        self.current += op_text.len();
        Ok(*op)
    }

    pub fn parse_station(&mut self) -> ParseResult<Station> {
        self.expect_exact_text("{")?;
        let name = self.expect_word()?;
        self.expect_exact_line("}")?;

        self.expect_exact_text("operation: ")?;
        let operation = self.parse_operation()?;
        self.expect_exact_line("")?;

        let mut next_num = 1;
        let mut output = vec![];
        while let Ok(line) = self.next_line() {
            if line.is_empty() { break }

            self.expect_exact_text(&format!("track {}: go to ", next_num))?;
            next_num += 1;

            output.push(self.parse_target()?);
            self.expect_exact_line("")?;
        }

        Ok(Station{name, operation, output})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_station() {
        let test = include_str!("../../../shunting_yard/test_station.train");
        let mut parser = Parser::new(test);
        match parser.parse_station() {
            Ok(station) => {
                println!("{:?}", station);
                let expected = Station {
                    name: String::from("Groningen"),
                    operation: Operation::Add,
                    output: vec![
                        Target { station: String::from("Groningen"), track: 0, span: Span{ start: 42, end: 59 } },
                        Target { station: String::from("Utrecht"), track: 0, span: Span{ start: 75, end: 90 } }
                    ]
                };
                assert_eq!(station, expected)
            }
            Err(_err) => {
                assert!(false);
            }
        }
    }
}