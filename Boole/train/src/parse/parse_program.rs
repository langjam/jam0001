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
                '\n' => self.current += 1,
                _ => {
                    return Err(ParserError {
                        span: Span::from_length(self.current, 1),
                        error: String::from("Expected the start of a new train or station here."),
                    })
                }
            }

        }

        Ok(Program{trains, stations})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_full() {
        let test = include_str!("../../../shunting_yard/test_full.train");
        let mut parser = Parser::new(test);
        match parser.parse_program() {
            Ok(program) => {
                println!("{:?}", program);
            }
            Err(err) => {
                err.print(test);
                assert!(false);
            }
        }
    }
}