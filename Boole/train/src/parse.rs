use crate::ast::{Program, Train, Station, Target, FirstClassPassenger, SecondClassPassenger};
use std::fmt::{Display, Formatter};
use crate::operations::Operation;
use std::collections::HashMap;
use strum::*;
use std::borrow::Borrow;

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


struct Parser<'a> {
    input: &'a str,
    current: usize,
}



impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser{input, current: 0}
    }
    fn rest(&self) -> &'a str {
        &self.input[self.current..]
    }

    fn expect_word(&mut self) -> ParseResult<String> {
        let line = self.rest().lines().next().unwrap();
        let word = line.split(|c: char| !c.is_alphanumeric()).next().unwrap();
        self.current += word.len();
        Ok(String::from(word))
    }

    fn expect_number(&mut self) -> ParseResult<isize> {
        let line = self.rest().lines().next().unwrap();
        let word = line.split_whitespace().next().unwrap();

        let num = word.parse::<isize>();
        if let Ok(num) = num {
            self.current += word.len();
            Ok(num)
        } else {
            Err(ParseError {
                span: Span::from_length(self.current, word.len()),
                error: format!("Expected a number here."),
            })
        }


    }

    fn expect_exact_text(&mut self, exact: &str) -> ParseResult<()> {
        let line = self.rest().lines().next().unwrap();
        if line.starts_with(exact) {
            self.current += exact.len();
            Ok(())
        } else {
            Err(ParseError {
                span: Span::from_length(self.current, 1),
                error: format!("Expected the exact text '{}' here.", exact)
            })
        }
    }

    fn expect_exact_line(&mut self, exact: &str) -> ParseResult<()> {
        let line = self.rest().lines().next().unwrap();
        if line == exact {
            self.current += line.len();
            self.current += 1; // Skip \n
            Ok(())
        } else {
            Err(ParseError {
                span: Span::from_length(self.current, line.len()),
                error: format!("Expected the exact line '{}' here.", exact)
            })
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct ParseError {
    span: Span,
    error: String,
}

impl ParseError {
    fn print(&self, input: &str) {
        println!("Parse error!");
        println!("At [{}-{}]: '{}'", self.span.start, self.span.end, &input[self.span.start..self.span.end]);
        println!("{}", self.error);
    }
}

type ParseResult<'a, T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> ParseResult<Program> {
        let mut trains = vec![];
        let mut stations=  vec![];

        while self.rest().len() > 0 {
            match self.rest().chars().next().unwrap() {
                '[' => trains.push(self.parse_train()?),
                '{' => stations.push(self.parse_station()?),
                '\n' => self.current += 1,
                _ => {
                    return Err(ParseError {
                        span: Span::from_length(self.current, 1),
                        error: String::from("Expected the start of a new train or station here."),
                    })
                }
            }

        }

        Ok(Program{trains, stations})
    }

    fn parse_target(&mut self) -> ParseResult<Target> {
        let station = self.expect_word()?;
        self.expect_exact_text(" track ")?;
        let track = (self.expect_number()? - 1) as usize;
        Ok(Target{ station, track })
    }

    fn parse_train(&mut self) -> ParseResult<Train> {
        self.expect_exact_line("[LOCOMOTIVE]")?;
        self.expect_exact_text("start at ")?;
        let start = self.parse_target()?;
        self.expect_exact_line("")?;

        self.expect_exact_line("[FIRST CLASS]")?;
        let mut first_class_passengers = vec![];
        while let Some(line) = self.rest().lines().next() {
            if line == "[SECOND CLASS]" { break }

            let res = line.splitn(2, ": ").collect::<Vec<_>>();
            if res.len() != 2 {
                return Err(ParseError {
                    span: Span::from_length(self.current, line.len()),
                    error: format!("Line must contain a `: ` to split name and text.")
                })
            } else {
                first_class_passengers.push(FirstClassPassenger {name: String::from(res[0]), data: String::from(res[1])})
            }
            self.current += line.len();
            self.current += 1;
        }


        self.expect_exact_line("[SECOND CLASS]")?;
        let mut second_class_passengers = vec![];
        while let Some(line) = self.rest().lines().next() {
            if line.is_empty() { break }
            let res = line.splitn(2, ": ").collect::<Vec<_>>();
            if res.len() != 2 {
                return Err(ParseError {
                    span: Span::from_length(self.current, line.len()),
                    error: format!("Line must contain a `: ` to split name and text.")
                })
            } else {
                if let Ok(data) = res[1].parse() {
                    second_class_passengers.push(SecondClassPassenger{name: String::from(res[0]), data})
                } else {
                    return Err(ParseError {
                        span: Span::from_length(self.current + res[0].len() + 2, res[1].len()),
                        error: format!("Data must be a number.")
                    })
                }
            }
            self.current += line.len();
            self.current += 1;
        }

        Ok(Train { start, first_class_passengers, second_class_passengers })
    }

    fn parse_operation(&mut self) -> ParseResult<Operation> {
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
                return Err(ParseError {
                    span: Span::from_length(self.current, op_text.len()),
                    error: format!("Operation not found.")
                })
            }
        };
        self.current += op_text.len();
        Ok(*op)
    }

    fn parse_station(&mut self) -> ParseResult<Station> {
        self.expect_exact_text("{")?;
        let name = self.expect_word()?;
        self.expect_exact_line("}")?;

        self.expect_exact_text("operation: ")?;
        let operation = self.parse_operation()?;
        self.expect_exact_line("")?;

        let mut next_num = 1;
        let mut output = vec![];
        while let Some(line) = self.rest().lines().next() {
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
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_train() {
        let test = include_str!("../../shunting_yard/test_train.train");
        let mut parser = Parser::new(test);
        match parser.parse_train() {
            Ok(train) => {
                println!("{:?}", train);
                let expected = Train {
                    start: Target { station: String::from("Groningen"), track: 0 },
                    first_class_passengers: vec![
                        FirstClassPassenger { name: String::from("robert"), data: String::from("this train is nice and red") },
                        FirstClassPassenger { name: String::from("peter"), data: String::from("this train must be at least") }
                    ],
                    second_class_passengers: vec![
                        SecondClassPassenger { name: String::from("jonathan"), data: 1 },
                        SecondClassPassenger { name: String::from("pietje"), data: -1 }
                    ] };
                assert_eq!(train, expected)
            }
            Err(err) => {
                err.print(test);
                assert!(false);
            }
        }
    }

    #[test]
    fn test_station() {
        let test = include_str!("../../shunting_yard/test_station.train");
        let mut parser = Parser::new(test);
        match parser.parse_station() {
            Ok(station) => {
                println!("{:?}", station);
                let expected = Station {
                    name: String::from("Groningen"),
                    operation: Operation::Add,
                    output: vec![
                        Target { station: String::from("Groningen"), track: 0 },
                        Target { station: String::from("Utrecht"), track: 0 }
                    ]
                };
                assert_eq!(station, expected)
            }
            Err(err) => {
                err.print(test);
                assert!(false);
            }
        }
    }
}