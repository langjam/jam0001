use warp::Filter;
use std::time::Duration;
use crate::frontend::Communicator;
use futures_util::stream::StreamExt;
use futures_util::SinkExt;
use futures_util::join;
use std::sync::mpsc::channel;
use train::interface::VmInterfaceMessage;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::sync::{Arc, MutexGuard};
use tokio::sync::Mutex;
use warp::ws::{Message, WebSocket};
use serde_json::Error;

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

async fn receive_message(ws: WebSocket, vm: Arc<Mutex<Communicator>>) {
    let (mut ws_tx, mut ws_rx) = ws.split();

    tokio::task::spawn(async move {
        while let Some(r) = ws_rx.next().await {
            match r {
                Ok(i) => match i.to_str() {
                    Ok(i) => {
                        log::info!("{}", i);
                        match serde_json::from_str::<MessageFromWebpage>(i) {
                            Ok(message) => match message {
                                MessageFromWebpage::AdvanceSimulation => {}
                                MessageFromWebpage::SendInputResponse(_, _) => {}
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

    let local_vm = vm.clone();
    tokio::task::spawn(async move {
        loop {
            let message = match local_vm.lock().await.rx.recv() {
                Ok(i) => i,
                Err(_) => {
                    break;
                }
            };

            let res = match message {
                VmInterfaceMessage::AskForInput(id) => {
                    serialize_silent!(&MessageToWebpage::AskForInput(id))
                }
                VmInterfaceMessage::Print(i) => {
                    serialize_silent!(&MessageToWebpage::Print(i))
                }
                VmInterfaceMessage::MoveTrain(_, _) => {
                    serialize_silent!(&MessageToWebpage::MoveTrain())
                }
                VmInterfaceMessage::EndSimulationStep => {
                    serialize_silent!(&MessageToWebpage::EndSimulationStep)
                }
            };

            if let Err(e) = ws_tx.send(res).await {
                log::error!("{}", e);
            }
        }
    });
}

pub async fn run(vm: Communicator) {
    let amvm = Arc::new(Mutex::new(vm));

    let ws = warp::path("ws")
        // The `ws()` filter will prepare the Websocket handshake.
        .and(warp::ws())
        .map(move |ws: warp::ws::Ws| {
            let amvm = amvm.clone();

            ws.on_upgrade(move |websocket| {
                let amvm = amvm.clone();
                receive_message(websocket, amvm)
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