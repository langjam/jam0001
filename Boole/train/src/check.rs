use crate::ast::{Program, Target};
use crate::parse::parser::{ParseResult, ParserError, Span};
use std::collections::{HashMap};

pub fn check<'a>(program: &Program, input: &str) -> ParseResult<()> {
    let stations = program.stations.iter().map(|s| (s.name.to_lowercase(), s)).collect::<HashMap<_, _>>();
    if program.stations.len() != stations.len() {
        return Err(ParserError {
            span: Span{start: 0, end: 0},
            error: format!("There is some station with a duplicate name."),
            input: input.to_string(),
        })
    }

    let check_target = |target: &Target| -> ParseResult<()> {
        if let Some(&station) = stations.get(&target.station.to_lowercase()) {
            if target.track >= station.operation.input_track_count() {
                return Err(ParserError {
                    span: target.span,
                    error: format!("Track does not exist at the station."),
                    input: input.to_string(),
                })
            }
        } else {
            return Err(ParserError {
                span: target.span,
                error: format!("Name does not exist."),
                input: input.to_string()
            })
        }
        Ok(())
    };

    //Check start locations
    for train in &program.trains {
        check_target(&train.start)?;

    }

    //Check goals
    for station in &program.stations {
        for target in &station.output {
            check_target(&target)?;
        }
    }


    Ok(())
}