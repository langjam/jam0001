use crate::ast::{Station, Train};


use thiserror::Error;
use std::string::FromUtf8Error;

#[derive(Debug, Error)]
pub enum CommunicatorError {
    #[error("failed to send over channel")]
    SendError,

    #[error("failed to convert train contents to utf8")]
    FromUTF8Error(#[from] FromUtf8Error)
}

#[async_trait::async_trait]
pub trait Communicator: Send + Sync {
    async fn ask_for_input(&self) -> Result<Vec<i64>, CommunicatorError>;
    fn print(&self, station: Station, data: Vec<i64>) -> Result<(), CommunicatorError>;
    fn print_char(&self, station: Station, data: Vec<i64>) -> Result<(), CommunicatorError>;
    fn move_train(&self, from_station:Station, to_station:Station, train:Train, start_track:usize, end_track:usize) -> Result<(), CommunicatorError>;
    fn delete_train(&self, train: Train) -> Result<(), CommunicatorError>;
}


