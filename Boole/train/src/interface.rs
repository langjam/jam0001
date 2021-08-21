use crate::ast::{Station, Train};
use std::sync::mpsc::{Sender, Receiver, channel, RecvError};
use std::sync::{Arc, Mutex};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicI64, Ordering};

pub struct CommunicatorError;

pub trait Communicator {
    fn ask_for_input(&self) -> Result<Vec<i64>, CommunicatorError>;
    fn print(&self, data: Vec<i64>) -> Result<(), CommunicatorError>;
    fn move_train(&self, from_station:Station, to_station:Station, train:Train, start_track:usize, end_track:usize);

    fn train_to_start(&self, start_station:Station, train:Train) -> Result<(), CommunicatorError>;
}


