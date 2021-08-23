use train::ast::{Program, Station, Train};
use warp::filters::ws::{WebSocket, Message};
use futures_util::stream::SplitSink;
use train::interface::{Communicator, CommunicatorError};
use serde::Serialize;
use train::operations::Operation;
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;
use std::io::{Write, Error};
use std::process::Command;
use crate::frontend::web::MessageToWebpage;
use futures_util::SinkExt;
use thiserror::Error;
use tokio::sync::mpsc::{UnboundedSender, Sender, channel};
use std::sync::atomic::{AtomicI64, Ordering};
use tokio::task::block_in_place;
use tokio::sync::Mutex;
use tokio::task;
use std::num::ParseIntError;
use std::time::Duration;

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
    channels: Mutex<HashMap<i64, Sender<Vec<i64>>>>,
    current_index: AtomicI64,
}

#[derive(Debug, Error)]
pub enum GenerateVisualizerDataError {
    #[error("failed to serialize: {0}")]
    Serialize(#[from] serde_json::Error),

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    #[error("generating visualizer data (in python) failed")]
    GenerateVisualizerData,

    #[error("parse error: {0}")]
    ParseNumber(#[from] ParseIntError),
}

impl WebRunner {
    pub fn new(program: Program, response_tx: UnboundedSender<MessageToWebpage>) -> Self {
        Self {
            program,
            response_tx,
            channels: Default::default(),
            current_index: AtomicI64::new(0),
        }
    }

    pub fn send(&self, m: MessageToWebpage) -> Result<(), CommunicatorError> {
        self.response_tx.send(m).map_err(|_| CommunicatorError::SendError)?;
        Ok(())
    }

    pub async fn input_response(&self, identifier: i64, message: Vec<i64>) {
        if let Some(i) = self.channels.lock().await.remove(&identifier) {
            log::info!("sending input response to sender");
            if let Err(e) = i.send(message).await {
                log::error!("{}", e);
            }
        }
    }

    pub async fn generate_visualizer_file(&self, connection_id: String) -> Result<PathBuf, GenerateVisualizerDataError> {
        let mut res = Vec::new();

        let mut indices = HashMap::new();

        for (index, i) in self.program.stations.iter().enumerate() {
            indices.insert(&i.name, index);
        }

        for station in &self.program.stations {
            let mut to = Vec::new();
            for i in &station.output {
                // make 1 indexed (again)
                to.push(vec![*indices.get(&i.station).expect("must exist"), i.track + 1])
            }

            res.push(VisualizerStation {
                r#type: station.operation,
                inputs: station.operation.input_track_count(),
                to,
                name: station.name.to_string(),
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
        // match procinfo::pid::status(child.id() as i32) {
        //     Ok(mem_status) => {
        //         let mem_size = mem_status.vm_size;
        //         log::info!("{} kb", mem_size);
        //     }
        //     Err(e) => {
        //         return Err(e.into());
        //     }
        // }

        log::info!("python id: {}", child.id());
        let memlimit = if let Ok(e) = std::env::var("PYTHON_MEMLIMIT") {
            e.parse()?
        } else {
            8000000
        };
        while let Ok(None) = child.try_wait() {
            tokio::time::sleep(Duration::from_secs(1)).await;
            let usage = WebRunner::python_mem_usage(child.id());

                log::info!("Python is using {} kb of memory ({} mb, {} gb)", usage, usage/1024, usage/1024/1024);


            if usage > memlimit {
                child.kill().unwrap();
            }
        }

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

    #[cfg(unix)]
    fn python_mem_usage(id: u32) -> i64 {
        let f = std::fs::read_to_string(format!("/proc/{}/status", id)).unwrap();
        let re = regex::Regex::new(r"VmSize:\s+(\d+)\s+kB").unwrap();
        if let Some(x) = re.captures(f.as_str()) {
            return x.get(1).unwrap().as_str().parse().unwrap();
        }
        -1
    }

    #[cfg(not(unix))]
    fn python_mem_usage(_: u32) -> i64 {
        -1
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

#[async_trait::async_trait]
impl Communicator for WebRunner {
    async fn ask_for_input(&self) -> Result<Vec<i64>, CommunicatorError> {
        let input_identifier = self.current_index.fetch_add(1, Ordering::SeqCst);

        let (tx, mut rx) = channel(1);

        self.channels.lock().await.insert(input_identifier, tx);
        self.send(MessageToWebpage::AskForInput { identifier: input_identifier })?;

        rx.recv().await.ok_or(CommunicatorError::SendError)
    }

    fn print(&self, station: Station, data: Vec<i64>) -> Result<(), CommunicatorError> {
        let mut str = String::new();
        for (i, data) in data.into_iter().enumerate() {
            if i != 0 {
                str += ", ";
            }
            str += &data.to_string();
        }

        self.send(MessageToWebpage::Print { station, message: str })
    }

    fn print_char(&self, station: Station, data: Vec<i64>) -> Result<(), CommunicatorError> {
        let char_data = data.iter().map(|x| (x & 0xFF) as u8).collect();
        let str = String::from_utf8(char_data)?;
        self.send(MessageToWebpage::Print { station, message: str })
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

    fn delete_train(&self, train: Train) -> Result<(), CommunicatorError> {
        self.send(MessageToWebpage::DeleteTrain { train })
    }
}