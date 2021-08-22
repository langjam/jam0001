use train::ast::{Program, Station, Train};
use warp::filters::ws::{WebSocket, Message};
use futures_util::stream::SplitSink;
use train::interface::{Communicator, CommunicatorError};
use serde::Serialize;
use train::operations::Operation;
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;
use std::io::Write;
use std::process::Command;
use crate::frontend::web::MessageToWebpage;
use futures_util::SinkExt;
use thiserror::Error;
use tokio::sync::mpsc::UnboundedSender;

#[derive(Serialize)]
struct VisualizerStation {
    r#type: Operation,
    inputs: usize,
    to: Vec<Vec<usize>>,
    name: String,
}

pub struct WebRunner {
    program: Program,
    response_tx: UnboundedSender<MessageToWebpage>,
}

#[derive(Debug, Error)]
pub enum GenerateVisualizerDataError {
    #[error("failed to serialize: {0}")]
    Serialize(#[from] serde_json::Error),

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    #[error("generating visualizer data (in python) failed")]
    GenerateVisualizerData,
}

impl WebRunner {
    pub fn new(program: Program, response_tx: UnboundedSender<MessageToWebpage>) -> Self {
        Self {
            program,
            response_tx,
        }
    }

    pub fn send(&self, m: MessageToWebpage) -> Result<(), CommunicatorError> {
        self.response_tx.send(m).map_err(|_| CommunicatorError::SendError)?;
        Ok(())
    }

    pub fn generate_visualizer_file(&self, connection_id: i64) -> Result<PathBuf, GenerateVisualizerDataError> {
        let mut res = Vec::new();

        let mut indices = HashMap::new();

        for (index, i) in self.program.stations.iter().enumerate() {
            indices.insert(&i.name, index);
        }

        for station in &self.program.stations {
            let mut to= Vec::new();
            for i in &station.output {
                // make 1 indexed (again)
                to.push(vec![*indices.get(&i.station).expect("must exist"), i.track + 1])
            }

            res.push(VisualizerStation {
                r#type: station.operation,
                inputs: station.operation.input_track_count(),
                to,
                name: station.name.to_string()
            })
        }

        let serialized = serde_json::to_string_pretty(&res)?;

        let mut path = PathBuf::new();
        path.push("visualizer");
        path.push("visualizer_setup");
        path.push(format!("{}.json", connection_id));


        if path.exists() {
            std::fs::remove_file(&path)?;
        }

        let mut file = File::create(&path)?;
        file.write_all(serialized.as_bytes())?;

        log::info!("calling python script to generate station layout");

        let python_executor = std::env::var("PYTHON_EXECUTABLE").unwrap_or("python".to_string());
        let mut command = Command::new(python_executor);
        command.arg("visualizer/router.py")
            .arg(format!("{:?}", &path));
        command.current_dir(".");

        let mut child = command.spawn()?;
        let status = child.wait()?;
        if !status.success() {
            return Err(GenerateVisualizerDataError::GenerateVisualizerData);
        }
        log::info!("finished generating station layout");


        let mut path = PathBuf::new();
        path.push("visualizer_setup");
        path.push(format!("{}.json.result.json", connection_id));

        Ok(path)
    }
}


pub async fn send(sender: &mut SplitSink<WebSocket, Message>, m: &MessageToWebpage) {
    let message = match serde_json::to_string(&m) {
        Ok(i) => Message::text(i),
        Err(e) => {
            log::error!("{}", e);
            return;
        }
    };

    if let Err(e) = sender.send(message).await {
        log::error!("{}", e);
    }
}

impl Communicator for WebRunner {
    fn ask_for_input(&self) -> Result<Vec<i64>, CommunicatorError> {
        todo!()
    }

    fn print(&self, data: Vec<i64>) -> Result<(), CommunicatorError> {
        self.send(MessageToWebpage::Print{message: data})
    }

    fn print_char(&self, data: Vec<i64>) -> Result<(), CommunicatorError> {
        self.send(MessageToWebpage::PrintChar{message: data})
    }

    fn move_train(&self, from_station: Station, to_station: Station, train: Train, start_track: usize, end_track: usize) -> Result<(), CommunicatorError> {
        self.send(MessageToWebpage::MoveTrain {
            from_station,
            to_station,
            train,
            start_track,
            end_track,
        })
    }
}