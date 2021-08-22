use crate::ast::{Station, Train};
use std::sync::mpsc::{Sender, Receiver, channel, RecvError, SendError};
use std::sync::{Arc, Mutex};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicI64, Ordering};
use thiserror::Error;
use std::string::FromUtf8Error;

#[derive(Debug, Error)]
pub enum CommunicatorError {
    #[error("failed to send over channel")]
    SendError,

    #[error("failed to convert train contents to utf8")]
    FromUTF8Error(#[from] FromUtf8Error)
}

impl<T> From<std::sync::mpsc::SendError<T>> for CommunicatorError {
    fn from(s: SendError<T>) -> Self {
        Self::SendError
    }
}

pub trait Communicator {
    fn ask_for_input(&self) -> Result<Vec<i64>, CommunicatorError>;
    fn print(&self, data: Vec<i64>) -> Result<(), CommunicatorError>;
    fn print_char(&self, data: Vec<i64>) -> Result<(), CommunicatorError>;
    fn move_train(&self, from_station:Station, to_station:Station, train:Train, start_track:usize, end_track:usize) -> Result<(), CommunicatorError>;
}


