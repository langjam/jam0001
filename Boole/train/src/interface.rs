use crate::ast::{Station, Train};
use std::sync::mpsc::{Sender, Receiver, channel, RecvError};
use std::sync::{Arc, Mutex};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicI64, Ordering};


pub struct Input {
    value: Vec<i64>
}

#[derive(Clone, Debug)]
pub enum VmInterfaceMessage {
    AskForInput(i64),
    Print(Vec<i64>),
    MoveTrain(Station, Station, Train, usize, usize),
    EndSimulationStep,
    StartTrain(Station, Train),
}

/// Interface for the virtual machine itself to send messages over to a frontend
pub struct VMInterface {
    sender: Sender<VmInterfaceMessage>,
    message_ids_in_use: Mutex<HashMap<i64, Sender<Input>>>,
    message_id: AtomicI64,
}

/// Error returned when the other side of the channel closed. VM should exit when receiving this.
pub struct OtherSideClosed;

impl VMInterface {
    pub fn new(sender: Sender<VmInterfaceMessage>) -> Self {
        Self {
            sender,
            message_ids_in_use: Mutex::new(Default::default()),
            message_id: Default::default()
        }
    }

    pub fn ask_for_input(&self) -> Result<Vec<i64>, OtherSideClosed> {
        let id = self.message_id.fetch_add(1, Ordering::SeqCst);
        self.sender.send(VmInterfaceMessage::AskForInput(id)).map_err(|_| OtherSideClosed)?;

        let (tx, rx) = channel();

        self.message_ids_in_use.lock().map_err(|_| OtherSideClosed)?.insert(id, tx);

        let res = match rx.recv() {
            Ok(i) => { i.value }
            Err(_) => {
                return Err(OtherSideClosed)
            }
        };

        let _ = self.message_ids_in_use.lock().map_err(|_| OtherSideClosed)?.remove(&id);

        Ok(res)
    }
    pub fn print(&self, data: Vec<i64>) -> Result<(), OtherSideClosed> {
        self.sender.send(VmInterfaceMessage::Print(data)).map_err(|_| OtherSideClosed)
    }
    pub fn move_train(&self, from_station:Station, to_station:Station, train:Train, start_track:usize, end_track:usize) -> Result<(), OtherSideClosed> {
        self.sender.send(VmInterfaceMessage::MoveTrain(from_station, to_station, train, start_track, end_track)).map_err(|_| OtherSideClosed)
    }

    pub fn train_to_start(&self, start_station:Station, train:Train) -> Result<(), OtherSideClosed>{
        self.sender.send(VmInterfaceMessage::StartTrain(start_station, train)).map_err(|_|OtherSideClosed)
    }
    pub fn end_simulation_step(&self) -> Result<(), OtherSideClosed> {
        self.sender.send(VmInterfaceMessage::EndSimulationStep).map_err(|_| OtherSideClosed)
    }
}