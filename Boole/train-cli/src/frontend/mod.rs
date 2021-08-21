use std::sync::mpsc::{Sender, Receiver, channel};

pub mod web;
pub mod cli;

//
// pub struct Communicator {
//     pub rx: Receiver<VmInterfaceMessage>
// }
//
// impl Communicator {
//     pub fn new() -> (Self, VMInterface) {
//         let (tx, rx) = channel();
//
//         (
//             Self {
//                 rx
//             },
//             VMInterface::new(tx)
//         )
//     }
// }