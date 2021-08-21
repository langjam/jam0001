use train::vm::Data;
use train::interface::Communicator;
use std::{thread, io};
use train::ast::{Station, Train};

pub(crate) struct CliRunner {}

impl CliRunner {
    pub fn new() -> Self {
        Self {

        }
    }

    pub fn run(&self, mut vm: Data) {
        loop {
            vm.do_current_step(self).expect("failed");

            let mut input_text = String::new();
            io::stdin()
                .read_line(&mut input_text)
                .expect("failed to read from stdin");
        }
    }
}

impl Communicator for CliRunner {
    fn ask_for_input(&self) -> Result<Vec<i64>, train::interface::CommunicatorError> {
        loop {
            let mut input_text = String::new();
            io::stdin()
                .read_line(&mut input_text)
                .expect("failed to read from stdin");

            let trimmed = input_text.trim();
            match trimmed.parse::<i64>() {
                Ok(i) => {
                    return Ok(vec![i]);
                },
                Err(..) => {
                    log::error!("this was not an integer: {}. retry", trimmed);
                },
            };
        }
    }

    fn print(&self, data: Vec<i64>) -> Result<(), train::interface::CommunicatorError> {
        log::info!("simulation says: {:?}", data);
        Ok(())
    }

    fn move_train(&self, from_station: Station, to_station: Station, train: Train, start_track: usize, end_track: usize) -> Result<(), train::interface::CommunicatorError> {
        log::info!("simulation says: train {} moved from {} to {}", train.identifier, from_station.name, to_station.name);
        Ok(())
    }
}