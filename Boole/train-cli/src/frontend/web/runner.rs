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

#[derive(Serialize)]
struct VisualizerStation {
    operation: Operation,
    inputs: usize,
    to: Vec<Vec<usize>>,
}

pub struct WebRunner {
    program: Program,
    sender: SplitSink<WebSocket, Message>,
}

impl WebRunner {
    pub fn new(program: Program, sender: SplitSink<WebSocket, Message>) -> Self {
        Self {
            program,
            sender
        }
    }

    pub fn generate_visualizer_file(&self, connection_id: i64) -> Result<PathBuf, ()> {
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
                operation: station.operation,
                inputs: station.operation.input_track_count(),
                to,
            })
        }

        let serialized = serde_json::to_string(&res).expect("failed to serialize program");

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

        let mut child = command.spawn().expect("spawn python program");
        let status = child.wait().expect("wasn't running");
        if !status.success() {
            log::error!("failed to generate station layout");
            return Err(())
        }


        let mut path = PathBuf::new();
        path.push("visualizer");
        path.push("visualizer_setup");
        path.push(format!("{}.result.json", connection_id));
        return Ok(path);
    }
}


impl Communicator for WebRunner {
    fn ask_for_input(&self) -> Result<Vec<i64>, CommunicatorError> {
        todo!()
    }

    fn print(&self, data: Vec<i64>) -> Result<(), CommunicatorError> {
        todo!()
    }

    fn move_train(&self, from_station: Station, to_station: Station, train: Train, start_track: usize, end_track: usize) -> Result<(), CommunicatorError> {
        todo!()
    }
}