pub mod runner;

use warp::Filter;
use std::time::Duration;
use futures_util::stream::{StreamExt, SplitSink};
use futures_util::SinkExt;
use futures_util::join;
use std::sync::mpsc::{channel, Sender};
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::sync::{Arc, MutexGuard};
use tokio::sync::Mutex;
use warp::ws::{Message, WebSocket};
use serde_json::Error;
use train::interface::{Communicator, CommunicatorError};
use train::vm::Data;
use train::ast::{Station, Train, Program};
use crate::frontend::cli::CliRunner;
use std::sync::atomic::{AtomicI64, Ordering};
use crate::frontend::web::runner::{WebRunner, send};

#[derive(Serialize)]
pub enum MessageToWebpage{
    AskForInput(i64),
    Print(Vec<i64>),
    PrintChar(Vec<i64>),
    MoveTrain{
        from_station: Station,
        to_station: Station,
        train: Train,
        start_track: usize,
        end_track: usize
    },
    VisualizerData(String),
    Error(String),
}

#[derive(Deserialize)]
pub enum MessageFromWebpage{
    AdvanceSimulation,
    SendInputResponse(i64, Vec<i64>)
}

async fn receive_message(ws: WebSocket, program: Program, connection_id: i64) {
    let (mut ws_tx, mut ws_rx) = ws.split();

    let (mut response_tx, response_rx) = channel();

    let mut runner = WebRunner::new(program.clone(), response_tx);
    let mut vm = Data::new(program);


    let visualizer_path = runner.generate_visualizer_file(connection_id);

    tokio::task::spawn(async move {
        while let Some(r) = ws_rx.next().await {
            match r {
                Ok(i) => match i.to_str() {
                    Ok(i) => {
                        log::info!("{}", i);
                        match serde_json::from_str::<MessageFromWebpage>(i) {
                            Ok(message) => match message {
                                MessageFromWebpage::AdvanceSimulation => {
                                    match vm.do_current_step(&runner) {
                                        Err(e) => runner.send(MessageToWebpage::Error(format!("{:?}", e))).expect("failed to send"),
                                        Ok(i) => {},
                                    }
                                }
                                MessageFromWebpage::SendInputResponse(id, response) => {
                                    // runner.input_response(id, response);
                                }
                            }
                            Err(e) => log::error!("{}", e)
                        }
                    }
                    Err(_) => log::error!("couldn't convert message to string (binary?)")
                }
                Err(e) => log::error!("{}", e),
            }
        }
    });

    tokio::task::spawn(async move {
        let rx = response_rx;

        send(&mut ws_tx, &MessageToWebpage::VisualizerData(format!("{:?}", visualizer_path))).await;

        loop {
            let res = rx.recv();

            if let Ok(i) = res {
                send(&mut ws_tx, &i).await;
            }
        }
    });
}

pub async fn run(program: Program) {
    let connection_id_counter = Arc::new(AtomicI64::new(0));

    let ws = warp::path("ws")
        // The `ws()` filter will prepare the Websocket handshake.
        .and(warp::ws())
        .map(move |ws: warp::ws::Ws| {
            let local_program = program.clone();
            let local_connection_id_counter = connection_id_counter.clone();
            ws.on_upgrade(move |websocket| {
                let connection_id = local_connection_id_counter.fetch_add(1, Ordering::SeqCst);

                receive_message(websocket, local_program.clone(), connection_id)
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