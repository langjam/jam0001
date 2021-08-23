use crate::parse::parser::*;
use crate::ast::*;

impl<'a> Parser<'a> {
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut trains = vec![];
        let mut stations=  vec![];

        while self.rest().len() > 0 {
            match self.rest().chars().next().unwrap() {
                '[' => trains.push(self.parse_train()?),
                '{' => stations.push(self.parse_station()?),
                '\r' => self.current += 1,
                '\n' => self.current += 1,
                _ => {
                    return Err(ParserError {
                        span: Span::from_length(self.current, 1),
                        error: String::from("Expected the start of a new train or station here."),
                        input: self.input.to_string(),
                    })
                }
            }

        }

        Ok(Program{trains, stations})
    }
}

#[cfg(test)]
mod tests {
    use crate::parse_and_check;

    #[test]
    fn test_full() {
        let test = include_str!("../../../shunting_yard/test_full.train");

        match parse_and_check(test) {
            Ok(program) => {
                println!("{:?}", program);
            }
            Err(_err) => {
                assert!(false);
            }
        }
    }
}