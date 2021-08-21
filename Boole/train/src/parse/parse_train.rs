use crate::parse::parser::*;
use crate::ast::*;

impl<'a> Parser<'a> {
    pub fn parse_target(&mut self) -> ParseResult<Target> {
        let start = self.current;
        let station = self.expect_word()?;
        self.expect_exact_text(" track ")?;
        let track = (self.expect_number()? - 1) as usize;
        Ok(Target{ station, track, span: Span{start, end: self.current } })
    }

    pub fn parse_train(&mut self) -> ParseResult<Train> {
        self.expect_exact_line("[LOCOMOTIVE]")?;
        self.expect_exact_text("start at ")?;
        let start = self.parse_target()?;
        self.expect_exact_line("")?;

        self.expect_exact_line("[FIRST CLASS]")?;
        let mut first_class_passengers = vec![];
        loop {
            let line = self.next_line()?;
            if line == "[SECOND CLASS]" { break }

            let res = line.splitn(2, ": ").collect::<Vec<_>>();
            if res.len() != 2 {
                return Err(ParserError {
                    span: Span::from_length(self.current, line.len()),
                    error: format!("Line must contain a `: ` to split name and text."),
                    input: self.input.to_string()
                })
            } else {
                first_class_passengers.push(FirstClassPassenger {name: String::from(res[0]), data: String::from(res[1])})
            }
            self.current += line.len();
            self.current += 1;
        }

        self.expect_exact_line("[SECOND CLASS]")?;
        let mut second_class_passengers = vec![];
        while let Ok(line) = self.next_line() {
            if line.is_empty() { break }
            let res = line.splitn(2, ": ").collect::<Vec<_>>();
            if res.len() != 2 {
                return Err(ParserError {
                    span: Span::from_length(self.current, line.len()),
                    error: format!("Line must contain a `: ` to split name and text."),
                    input: self.input.to_string(),
                })
            } else {
                if let Ok(data) = res[1].parse() {
                    second_class_passengers.push(SecondClassPassenger{name: String::from(res[0]), data})
                } else {
                    return Err(ParserError {
                        span: Span::from_length(self.current + res[0].len() + 2, res[1].len()),
                        error: format!("Data must be a number."),
                        input: self.input.to_string(),
                    })
                }
            }
            self.current += line.len();
            self.current += 1;
        }

        Ok(Train { start, first_class_passengers, second_class_passengers })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_train() {
        let test = include_str!("../../../shunting_yard/test_train.train");
        let mut parser = Parser::new(test);
        match parser.parse_train() {
            Ok(train) => {
                println!("{:?}", train);
                let expected = Train {
                    start: Target { station: String::from("Groningen"), track: 0, span: Span{ start: 22, end: 39 } },
                    first_class_passengers: vec![
                        FirstClassPassenger { name: String::from("robert"), data: String::from("this train is nice and red") },
                        FirstClassPassenger { name: String::from("peter"), data: String::from("this train must be at least") }
                    ],
                    second_class_passengers: vec![
                        SecondClassPassenger { name: String::from("jonathan"), data: 1 },
                        SecondClassPassenger { name: String::from("pietje"), data: -1 }
                    ]
                };
                assert_eq!(train, expected)
            }
            Err(err) => {
                assert!(false);
            }
        }
    }
}