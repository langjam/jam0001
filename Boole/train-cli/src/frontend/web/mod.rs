pub mod runner;

use warp::Filter;
use std::time::Duration;
use futures_util::stream::StreamExt;
use futures_util::join;
use tokio::sync::mpsc::{unbounded_channel, channel};
use serde::{Serialize, Deserialize};
use std::sync::{Arc};
use warp::ws::WebSocket;
use train::vm::Data;
use train::ast::{Station, Train, Program};
use std::sync::atomic::{AtomicI64, Ordering};
use crate::frontend::web::runner::{WebRunner, send, GenerateVisualizerDataError};
use thiserror::Error;
use train::interface::Communicator;
use std::path::PathBuf;

#[derive(Serialize)]
#[serde(tag = "type")]
pub enum MessageToWebpage {
    AskForInput {
        identifier: i64,
    },
    Print {
        message: String,
        station: Station,
    },
    MoveTrain {
        from_station: Station,
        to_station: Station,
        train: Train,
        start_track: usize,
        end_track: usize,
    },
    VisualizerData {
        path: String,
    },
    Error {
        message: String
    },
    CreateDataError,
    DeleteTrain {
        train: Train
    },
}

#[derive(Deserialize)]
#[serde(tag = "type")]
pub enum MessageFromWebpage {
    AdvanceSimulation,
    SendInputResponse {
        identifier: i64,
        input: Vec<i64>,
    },
}

#[derive(Debug, Error)]
pub enum ReceiveMessageError {
    #[error("generate visualizer data: {0}")]
    GenerateVisualizerDataError(#[from] GenerateVisualizerDataError)
}

async fn receive_message_wrapper(ws: WebSocket, program: Program, connection_id: String, reuse: bool) {
    if let Err(e) = receive_message(ws, program, connection_id, reuse).await {
        log::error!("wrapper {}", e);
    }
}


async fn receive_message(ws: WebSocket, program: Program, connection_id: String, reuse: bool) -> Result<(), ReceiveMessageError> {
    let (mut ws_tx, mut ws_rx) = ws.split();

    let (response_tx, response_rx) = unbounded_channel();

    let (visualizer_path_tx, mut visualizer_path_rx) = channel(1);

    tokio::task::spawn(async move {
        let runner = Arc::new(WebRunner::new(program.clone(), response_tx));
        let mut vm = Arc::new(Data::new(program).await);

        let mut check_path = PathBuf::new();
        check_path.push("visualizer");
        check_path.push("visualizer_setup");
        check_path.push(format!("{}.json.result.json", connection_id));
        let visualizer_res =
            if check_path.exists() && reuse {
                let mut check_path = PathBuf::new();
                check_path.push("visualizer_setup");
                check_path.push(format!("{}.json.result.json", connection_id));
                Ok(check_path)
            } else {
                runner.generate_visualizer_file(connection_id).await
            };


        let visualizer_path = match visualizer_res {
            Ok(i) => i,
            Err(e) => {
                let local_runner = runner.clone();
                tokio::task::spawn(async move {
                    if let Err(e) = local_runner.send(MessageToWebpage::CreateDataError) {
                        log::error!("{}", e)
                    }
                });
                return;
            }
        };

        tokio::task::spawn(async move {
            if let Err(e) = visualizer_path_tx.send(visualizer_path).await {
                log::error!("{}", e)
            }
        });

        while let Some(r) = ws_rx.next().await {
            match r {
                Ok(i) => match i.to_str() {
                    Ok(i) => {
                        log::info!("{}", i);
                        match serde_json::from_str::<MessageFromWebpage>(i) {
                            Ok(message) => match message {
                                MessageFromWebpage::AdvanceSimulation => {
                                    let local_vm = vm.clone();
                                    let local_runner = runner.clone();
                                    tokio::task::spawn(async move {
                                        let step = local_vm.do_current_step(
                                            local_runner.clone()
                                        );
                                        let res = step.await;
                                        match res {
                                            Err(e) => {
                                                local_runner.send(MessageToWebpage::Error { message: format!("{:?}", e) }).expect("failed to send")
                                            }
                                            Ok(_) => {}
                                        }
                                    });
                                }
                                MessageFromWebpage::SendInputResponse { identifier, input } => {
                                    runner.input_response(identifier, input).await;
                                }
                            }
                            Err(e) => {
                                log::error!("serde: {}", e);
                                runner.send(MessageToWebpage::Error { message: format!("{:?} (vm crashed, please reload simulation)", e) }).expect("failed to send")
                            }
                        }
                    }
                    Err(_) => log::error!("couldn't convert message to string (binary?)")
                }
                Err(e) => log::error!("to string: {}", e),
            }
        }
    });

    tokio::task::spawn(async move {
        let mut rx = response_rx;

        if let Some(visualizer_path) = visualizer_path_rx.recv().await {
            send(&mut ws_tx, &MessageToWebpage::VisualizerData { path: format!("{:?}", visualizer_path) }).await;
        }

        loop {
            let res = rx.recv().await;

            match res {
                Some(i) => send(&mut ws_tx, &i).await,
                None => {
                    log::error!("receive error: (disconnected)");
                    break;
                }
            }
        }
    });

    Ok(())
}

pub async fn run(program: Program, reuse: bool, hash: String) {
    let ws = warp::path("ws")
        // The `ws()` filter will prepare the Websocket handshake.
        .and(warp::ws())
        .map(move |ws: warp::ws::Ws| {
            let local_program = program.clone();
            let hclone = hash.clone();
            ws.on_upgrade(move |websocket| {
                receive_message_wrapper(websocket, local_program.clone(), hclone.clone(), reuse)
            })
        });

    let cors = warp::cors()
        .allow_any_origin()
        .allow_headers(vec!["*"])
        .allow_methods(vec!["*"]);

    let static_files = warp::get().and(warp::fs::dir("visualizer"));

    let routes = static_files.or(ws).with(cors);

    let timeout = tokio::time::timeout(Duration::new(1, 0), async {
        open::that("http://localhost:12345?wsurl=ws%3A%2F%2Flocalhost%3A12345%2Fws").expect("couldn't open url");
    });
    let serve = warp::serve(routes).run(([127, 0, 0, 1], 12345));

    let (_, _) = join!(timeout, serve);
}