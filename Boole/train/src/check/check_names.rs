use crate::ast::Program;
use crate::parse::parser::{ParseResult, ParserError};
use std::collections::{HashSet, HashMap};

pub fn check_names(program: &Program) -> ParseResult<()> {
    let stations = program.stations.iter().map(|s| (s.name.to_lowercase(), s)).collect::<HashMap<_, _>>();

    //Check start locations
    for train in &program.trains {
        if !stations.contains_key(&train.start.station.to_lowercase()) {
            return Err(ParserError {
                span: train.start.span,
                error: format!("Name does not exist.")
            })
        }
    }

    //Check goals
    for station in &program.stations {
        for target in &station.output {
            if !stations.contains_key(&target.station.to_lowercase()) {
                return Err(ParserError {
                    span: target.span,
                    error: format!("Name does not exist.")
                })
            }
        }
    }


    Ok(())
}