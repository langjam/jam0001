use train::vm::Data;
use train::interface::{Communicator, CommunicatorError};
use std::io;
use train::ast::{Station, Train};
use std::sync::Arc;

pub(crate) struct CliRunner {}

impl CliRunner {
    pub fn new() -> Self {
        Self {

        }
    }

    pub async fn run(self, mut vm: Data) {
        let arc_self = Arc::new(self);
        loop {
            log::debug!("Next iteration!");
            if !vm.do_current_step(arc_self.clone()).await.expect("failed") {
                break
            }

            let mut input_text = String::new();
            io::stdin()
                .read_line(&mut input_text)
                .expect("failed to read from stdin");
        }
    }
}

#[async_trait::async_trait]
impl Communicator for CliRunner {
    async fn ask_for_input(&self) -> Result<Vec<i64>, train::interface::CommunicatorError> {
        loop {
            let mut input_text = String::new();
            log::info!("INPUT: ");
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

    fn print(&self, station: Station, data: Vec<i64>) -> Result<(), train::interface::CommunicatorError> {
        log::info!("OUTPUT: {:?}", data);
        Ok(())
    }

    fn print_char(&self, station: Station, data: Vec<i64>) -> Result<(), CommunicatorError> {
        let char_data = data.iter().map(|x| (x&0xFF) as u8).collect();
        log::info!("OUTPUT: {}", String::from_utf8(char_data)?);
        Ok(())
    }

    fn move_train(&self, from_station: Station, to_station: Station, train: Train, start_track: usize, end_track: usize) -> Result<(), train::interface::CommunicatorError> {
        log::debug!("simulation says: train {} {:?} moved from ({} track {}) to ({} track {})", train.identifier, train.second_class_passengers, from_station.name, start_track, to_station.name, end_track);
        Ok(())
    }

    fn delete_train(&self, train: Train) -> Result<(), CommunicatorError> {
        Ok(())
    }
}