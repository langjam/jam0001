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
use crate::frontend::web::runner::WebRunner;

#[derive(Serialize)]
enum MessageToWebpage{
    AskForInput(i64),
    Print(Vec<i64>),
    MoveTrain(),
    EndSimulationStep,
}

#[derive(Deserialize)]
enum MessageFromWebpage{
    AdvanceSimulation,
    SendInputResponse(i64, Vec<i64>)
}

macro_rules! serialize_silent {
    ($($m: tt)*) => {
        match serde_json::to_string($($m)*) {
            Ok(i) => Message::text(i),
            Err(e) => {
                log::error!("{}", e);
                continue;
            }
        }
    };
}

async fn receive_message(ws: WebSocket, program: Program, connection_id: i64) {
    let (mut ws_tx, mut ws_rx) = ws.split();

    let runner = WebRunner::new(program.clone(), ws_tx);
    let vm = Data::new(program);


    runner.generate_visualizer_file(connection_id);

    tokio::task::spawn(async move {
        while let Some(r) = ws_rx.next().await {
            // match r {
            //     Ok(i) => match i.to_str() {
            //         Ok(i) => {
            //             log::info!("{}", i);
            //             match serde_json::from_str::<MessageFromWebpage>(i) {
            //                 Ok(message) => match message {
            //                     MessageFromWebpage::AdvanceSimulation => {}
            //                     MessageFromWebpage::SendInputResponse(_, _) => {}
            //                 }
            //                 Err(e) => log::error!("{}", e)
            //             }
            //         }
            //         Err(_) => log::error!("couldn't convert message to string (binary?)")
            //     }
            //     Err(e) => log::error!("{}", e),
            // }
        }
    });

    tokio::task::spawn(async move {
        loop {
            // let message = match local_vm.lock().await.rx.recv() {
            //     Ok(i) => i,
            //     Err(_) => {
            //         break;
            //     }
            // };

            // let res = match message {
            //     VmInterfaceMessage::AskForInput(id) => {
            //         serialize_silent!(&MessageToWebpage::AskForInput(id))
            //     }
            //     VmInterfaceMessage::Print(i) => {
            //         serialize_silent!(&MessageToWebpage::Print(i))
            //     }
            //     VmInterfaceMessage::MoveTrain(_, _) => {
            //         serialize_silent!(&MessageToWebpage::MoveTrain())
            //     }
            //     VmInterfaceMessage::EndSimulationStep => {
            //         serialize_silent!(&MessageToWebpage::EndSimulationStep)
            //     }
            // };

            // if let Err(e) = ws_tx.send(res).await {
            //     log::error!("{}", e);
            // }
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