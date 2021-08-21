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
use std::process::{Command, ExitStatus};
use crate::frontend::web::MessageToWebpage;
use futures_util::SinkExt;
use std::sync::mpsc::Sender;

#[derive(Serialize)]
struct VisualizerStation {
    r#type: Operation,
    inputs: usize,
    to: Vec<Vec<usize>>,
}

pub struct WebRunner {
    program: Program,
    response_tx: Sender<MessageToWebpage>,
}


impl WebRunner {
    pub fn new(program: Program, response_tx: Sender<MessageToWebpage>) -> Self {
        Self {
            program,
            response_tx,
        }
    }

    pub fn send(&self, m: MessageToWebpage) -> Result<(), CommunicatorError> {
        self.response_tx.send(m).map_err(|_| CommunicatorError)?;
        Ok(())
    }

    pub fn generate_visualizer_file(&self, connection_id: i64) -> PathBuf {
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
            })
        }

        let serialized = serde_json::to_string_pretty(&res).expect("failed to serialize program");

        let mut path = PathBuf::new();
        path.push("visualizer");
        path.push("visualizer_setup");
        path.push(format!("{}.json", connection_id));


        if path.exists() {
            std::fs::remove_file(&path).expect("coluld not remove file");
        }

        let mut file = File::create(&path).expect("could not create file");
        file.write_all(serialized.as_bytes()).expect("failed to write");

        log::info!("calling python script to generate station layout");

        let python_executor = std::env::var("PYTHON_EXECUTABLE").unwrap_or("python".to_string());
        let mut command = Command::new(python_executor);
        command.arg("visualizer/router.py")
            .arg(format!("{:?}", &path));
        command.current_dir(".");

        let mut child = command.spawn().expect("spawn python program");
        let status = child.wait().expect("wasn't running");
        if !status.success() {
            panic!("failed to generate station layout");
        }
        log::info!("finished generating station layout");


        let mut path = PathBuf::new();
        path.push("visualizer_setup");
        path.push(format!("{}.json.result.json", connection_id));

        path
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