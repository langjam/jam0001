use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use std::sync::{Arc, MutexGuard, PoisonError};
use tokio::sync::Mutex;

use crate::ast::{Program, Station, Train, SecondClassPassenger};
use crate::interface::{Communicator, CommunicatorError};
use crate::operations::Operation;

#[derive(Debug)]
pub enum VMError {
    ConnectionClosed,
    UnlockError,
    PassengerMissing,
}

impl<T> From<PoisonError<MutexGuard<'_, T>>> for VMError {
    fn from(_: PoisonError<MutexGuard<T>>) -> Self {
        Self::UnlockError
    }
}

impl From<CommunicatorError> for VMError {
    fn from(_: CommunicatorError) -> Self {
        Self::ConnectionClosed
    }
}

#[derive(Clone)]
struct TrainData {
    train: Train,
    station_name: String,
    track: usize,
}

impl TrainData {
    fn new(train: Train) -> Arc<Mutex<TrainData>> {
        let st = train.start.station.clone();
        let tr = train.start.track.clone();
        let td = Self {
            train: train.clone(),
            station_name: st.clone(),
            track: tr.clone(),
        };
        Arc::new(Mutex::new(td))
    }

    #[cfg(test)]
    fn first_passenger_value(&self) -> Option<i64> {
        let data = self.train.second_class_passengers.first()?.data;
        Some(data)
    }
}

pub struct StationData {
    trains: [VecDeque<Arc<Mutex<TrainData>>>; 2],
    station: Station,
}

impl StationData {
    fn new(station: Station) -> Self {
        Self {
            trains: [VecDeque::new(), VecDeque::new()],
            station,
        }
    }
}

impl Deref for StationData {
    type Target = Station;

    fn deref(&self) -> &Self::Target {
        &self.station
    }
}

#[allow(dead_code)]
pub struct Data {
    stations: HashMap<String, Arc<Mutex<StationData>>>,
    trains: Vec<Arc<Mutex<TrainData>>>,
}

impl Data {
    pub async fn new(program: Program) -> Data {
        let mut stations = HashMap::new();
        for station in program.stations {
            stations.insert(
                station.clone().name,
                Arc::new(Mutex::new(StationData::new(station))),
            );
        }
        let mut trains = vec![];
        for train in program.trains {
            let td = TrainData::new(train.clone());
            trains.push(td.clone());
            let target = train.start;
            let mut st = stations
                .get(target.station.as_str())
                .unwrap()
                .lock()
                .await;
            st.trains[target.track].push_back(td);
        }
        Self { stations, trains }
    }

    pub async fn train_count(&self) -> Result<usize, VMError> {
        let mut x = 0;
        for (_, station) in self.stations.iter() {
            let st = station.lock().await;
            x += st.trains.iter().map(|x| x.len()).sum::<usize>();
        }
        Ok(x)
    }

    pub async fn do_current_step(&self, interface: Arc<dyn Communicator>) -> Result<bool, VMError> {
        let mut targets = vec![];
        let mut did_any_work = false;
        for (_, station_arc) in self.stations.iter() {
            let mut station = station_arc.lock().await;
            let mut did_work = false;
            match station.operation {
                Operation::Nothing => {
                    did_work = true;
                }
                Operation::PrintNumber => {
                    if let Some(x) = station.trains[0].front() {
                        let zz = x.lock().await;
                        if let Ok(_) = interface.print(
                            station.station.clone(),
                            zz.train
                                .second_class_passengers
                                .iter()
                                .map(|x| x.data)
                                .collect(),
                        ) {
                            did_work = true;
                        } else {
                            return Err(VMError::ConnectionClosed);
                        }
                    }
                }
                Operation::PrintString => {
                    if let Some(x) = station.trains[0].front() {
                        let zz = x.lock().await;
                        if let Ok(_) = interface.print_char(
                            station.station.clone(),
                            zz.train
                                .second_class_passengers
                                .iter()
                                .map(|x| x.data)
                                .collect(),
                        ) {
                            did_work = true;
                        } else {
                            return Err(VMError::ConnectionClosed);
                        }
                    }
                }
                Operation::Input => {
                    if let Some(x) = station.trains[0].front() {
                        let mut t = x.lock().await;
                        if let Ok(other) = interface.ask_for_input().await {
                            for num in other {
                                t.train.second_class_passengers.push(SecondClassPassenger { name: String::from("input"), data: num });
                                t.train.second_class_passengers.rotate_right(1);
                            }
                            did_work = true;
                        } else {
                            return Err(VMError::ConnectionClosed);
                        }
                    }
                }
                Operation::SwitchEqZero => {
                    if let Some(x) = station.trains[0].pop_front() {
                        let xcln = x.clone();
                        let st = x.lock().await;
                        let val = st
                            .train
                            .second_class_passengers
                            .first()
                            .ok_or(VMError::PassengerMissing)?;
                        if val.data == 0 {
                            let target = station.output.clone()[0].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                            did_work = true;
                        } else {
                            did_work = true;
                            let target = station.output.clone()[1].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                        }
                    }
                }
                Operation::SwitchGteZero => {
                    if let Some(x) = station.trains[0].pop_front() {
                        let xcln = x.clone();
                        let st = x.lock().await;
                        let val = st
                            .train
                            .second_class_passengers
                            .first()
                            .ok_or(VMError::PassengerMissing)?;
                        if val.data >= 0 {
                            let target = station.output.clone()[0].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                            did_work = true;
                        } else {
                            let target = station.output.clone()[1].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                            did_work = true;
                        }
                    }
                }
                Operation::SwitchEmpty => {
                    if let Some(x) = station.trains[0].pop_front() {
                        let xcln = x.clone();
                        let st = x.lock().await;
                        let val = st.train.second_class_passengers.len();
                        if val == 0 {
                            let target = station.output.clone()[0].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                            did_work = true;
                        } else {
                            let target = station.output.clone()[1].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                            did_work = true;
                        }
                    }
                }
                Operation::Duplicate => {
                    if let Some(x) = station.trains[0].front().cloned() {
                        let train = x.lock().await;
                        let mut t2 = train.clone();
                        t2.train.identifier = rand::random();
                        station.trains[1].push_back(Arc::new(Mutex::new(t2)));
                        did_work = true;
                    }
                }
                Operation::Rotate => {
                    if let Some(x) = station.trains[0].front() {
                        let mut xx = x.lock().await;
                        xx.train.second_class_passengers.rotate_left(1);
                        did_work = true;
                    }
                }
                Operation::DeleteTop => {
                    if let Some(x) = station.trains[0].front() {
                        let mut xx = x.lock().await;
                        xx.train.second_class_passengers.remove(0);
                        did_work = true;
                    }
                }
                Operation::Transfer => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let xx = {
                                let mut z = x.lock().await;
                                z.train.second_class_passengers.remove(0)
                            };
                            let mut yy = y.lock().await;
                            yy.train.second_class_passengers.push(xx);
                            yy.train.second_class_passengers.rotate_right(1);
                            did_work = true;
                        }
                    }
                }
                Operation::Add => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().await;
                            let yy = y.lock().await;
                            let mut first_data = xx
                                .train
                                .second_class_passengers
                                .get_mut(0)
                                .ok_or(VMError::PassengerMissing)?;
                            let second_data = yy
                                .train
                                .second_class_passengers
                                .first()
                                .ok_or(VMError::PassengerMissing)?
                                .data;
                            first_data.data += second_data;
                            did_work = true;
                        }
                    }
                }
                Operation::Sub => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().await;
                            let yy = y.lock().await;
                            let mut first_data = xx
                                .train
                                .second_class_passengers
                                .get_mut(0)
                                .ok_or(VMError::PassengerMissing)?;
                            let second_data = yy
                                .train
                                .second_class_passengers
                                .first()
                                .ok_or(VMError::PassengerMissing)?
                                .data;
                            first_data.data -= second_data;
                            did_work = true;
                        }
                    }
                }
                Operation::Mul => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().await;
                            let yy = y.lock().await;
                            let mut first_data = xx
                                .train
                                .second_class_passengers
                                .get_mut(0)
                                .ok_or(VMError::PassengerMissing)?;
                            let second_data = yy
                                .train
                                .second_class_passengers
                                .first()
                                .ok_or(VMError::PassengerMissing)?
                                .data;
                            first_data.data *= second_data;
                            did_work = true;
                        }
                    }
                }
                Operation::Div => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().await;
                            let yy = y.lock().await;
                            let mut first_data = xx
                                .train
                                .second_class_passengers
                                .get_mut(0)
                                .ok_or(VMError::PassengerMissing)?;
                            let second_data = yy
                                .train
                                .second_class_passengers
                                .first()
                                .ok_or(VMError::PassengerMissing)?
                                .data;
                            first_data.data /= second_data;
                            did_work = true;
                        }
                    }
                }
                Operation::Mod => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().await;
                            let yy = y.lock().await;
                            let mut first_data = xx
                                .train
                                .second_class_passengers
                                .get_mut(0)
                                .ok_or(VMError::PassengerMissing)?;
                            let second_data = yy
                                .train
                                .second_class_passengers
                                .first()
                                .ok_or(VMError::PassengerMissing)?
                                .data;
                            first_data.data %= second_data;
                            did_work = true;
                        }
                    }
                }
                Operation::Delete => {
                    let st = &mut station.trains[0];
                    while !st.is_empty() {
                        let pop = st.pop_front().ok_or(VMError::UnlockError)?;
                        let train = pop.lock().await;
                        interface.delete_train(train.train.clone())?;
                    }
                    did_work = true;
                }
            }

            if station.operation != Operation::Delete
                && station.operation != Operation::SwitchGteZero
                && station.operation != Operation::SwitchEqZero
                && station.operation != Operation::SwitchEmpty
                && did_work
            {
                {
                    if let Some(x) = station.trains[0].pop_front() {
                        let target = station.output.clone()[0].clone();
                        targets.push((x, target, station.station.clone(), 0));
                    }
                }

                {
                    if let Some(x) = station.trains[1].pop_front() {
                        let target = station.output.clone()[1].clone();
                        targets.push((x, target, station.station.clone(), 1));
                    }
                }
            }
        }

        for (train, target, current, track) in targets {
            let mut station = self
                .stations
                .get(target.station.as_str())
                .ok_or(VMError::PassengerMissing)?
                .lock().await;
            {
                let tr = train.lock().await;
                interface
                    .move_train(
                        current,
                        station.station.clone(),
                        tr.train.clone(),
                        track,
                        target.track,
                    )
                    .unwrap_or(());
            }
            station.trains[target.track].push_back(train);
            did_any_work = true;
        }

        Ok(did_any_work)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Program, SecondClassPassenger, Station, Target, Train};
    use crate::interface::{Communicator, CommunicatorError};
    use crate::operations::Operation;
    use crate::parse::parser::Span;
    use crate::vm::Data;
    use crate::wishes::{ColorChoice, TrainConfig};
    use std::sync::Arc;


    struct VMInterface(bool);

    impl VMInterface {
        fn new() -> Self {
            Self(false)
        }
        fn new_print() -> Self {
            Self(true)
        }
    }

    #[async_trait::async_trait]
    impl Communicator for VMInterface {
        async fn ask_for_input(&self) -> Result<Vec<i64>, CommunicatorError> {
            Ok(vec![0])
        }

        fn print(&self, _: Station, _data: Vec<i64>) -> Result<(), CommunicatorError> {
            Ok(())
        }

        fn print_char(&self, _: Station, _data: Vec<i64>) -> Result<(), CommunicatorError> {
            Ok(())
        }

        fn move_train(
            &self,
            _from_station: Station,
            _to_station: Station,
            _train: Train,
            _start_track: usize,
            _end_track: usize,
        ) -> Result<(), CommunicatorError> {
            Ok(())
        }

        fn delete_train(&self, _train: Train) -> Result<(), CommunicatorError> {
            Ok(())
        }
    }

    macro_rules! create_program {
        ($x:expr) => {
            Program {
                trains: vec![
                    Train {
                        identifier: 0,
                        config: TrainConfig {
                            primary_color: ColorChoice::LightRed,
                            secondary_color: ColorChoice::DarkRed,
                            length: 1,
                        },
                        start: Target {
                            span: Span::from_length(0, 1),
                            track: 0,
                            station: "Test".to_string(),
                        },
                        first_class_passengers: vec![],
                        second_class_passengers: vec![SecondClassPassenger {
                            data: 10,
                            name: "Kees".to_string(),
                        }],
                    },
                    Train {
                        identifier: 0,
                        config: TrainConfig {
                            primary_color: ColorChoice::LightRed,
                            secondary_color: ColorChoice::DarkRed,
                            length: 1,
                        },
                        start: Target {
                            span: Span::from_length(0, 1),
                            track: 1,
                            station: "Test".to_string(),
                        },
                        first_class_passengers: vec![],
                        second_class_passengers: vec![SecondClassPassenger {
                            data: 20,
                            name: "Pieter".to_string(),
                        }],
                    },
                ],
                stations: vec![Station {
                    name: "Test".to_string(),
                    operation: $x,
                    output: vec![
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 0,
                        },
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 1,
                        },
                    ],
                }],
            }
        };
    }

    macro_rules! bin_ops {
        ($func:ident, $x:expr, $op:tt) => {
            #[tokio::test]
            async fn $func() {
                let program = create_program!($x);
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
                pp.do_current_step(i).await.unwrap();
                let train = pp.trains[0].lock().await;
                assert_eq!(Some(10 $op 20), train.first_passenger_value());
            }
        };
    }

    #[tokio::test]
    async fn nothing_does_nothing() {
        let program = create_program!(Operation::Nothing);
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        assert_eq!(pp.train_count().await.unwrap(), 2);
        pp.do_current_step(i).await.unwrap();
        let train = pp.trains[0].lock().await;
        assert_eq!(Some(10), train.first_passenger_value());
    }

    #[tokio::test]
    async fn trains_travel() {
        let program = Program {
            trains: vec![Train {
                identifier: 0,
                config: TrainConfig {
                    primary_color: ColorChoice::LightRed,
                    secondary_color: ColorChoice::DarkRed,
                    length: 1,
                },
                start: Target {
                    span: Span::from_length(0, 1),
                    track: 0,
                    station: "Test".to_string(),
                },
                first_class_passengers: vec![],
                second_class_passengers: vec![SecondClassPassenger {
                    data: 10,
                    name: "Kees".to_string(),
                }],
            }],
            stations: vec![
                Station {
                    name: "Test".to_string(),
                    operation: Operation::Nothing,
                    output: vec![
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test2".to_string(),
                            track: 0,
                        },
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test2".to_string(),
                            track: 1,
                        },
                    ],
                },
                Station {
                    name: "Test2".to_string(),
                    operation: Operation::Nothing,
                    output: vec![
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 1,
                        },
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 0,
                        },
                    ],
                },
            ],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        {
            pp.do_current_step(i.clone()).await.unwrap();
            let station = pp.stations.get("Test2").unwrap().lock().await;
            assert_eq!(station.trains[0].len(), 1);
        }
        pp.do_current_step(i.clone()).await.unwrap();
        {
            let station = pp.stations.get("Test").unwrap().lock().await;
            assert_eq!(station.trains[1].len(), 1);
        }
        pp.do_current_step(i.clone()).await.unwrap();
        {
            let station = pp.stations.get("Test2").unwrap().lock().await;
            assert_eq!(station.trains[1].len(), 1);
        }
        pp.do_current_step(i.clone()).await.unwrap();
        {
            let station = pp.stations.get("Test").unwrap().lock().await;
            assert_eq!(station.trains[0].len(), 1);
        }
        // while let Ok(t) = receiver.recv_timeout(Duration::from_millis(10)) {
        //     println!("{:?}", t);
        // }
    }

    #[tokio::test]
    async fn print_prints() {
        let program = create_program!(Operation::PrintString);
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        pp.do_current_step(i).await.unwrap();
        // receiver.recv().unwrap();
        // receiver.recv().unwrap();
        // if let Ok(m) = receiver.recv() {
        //     match m {
        //         VmInterfaceMessage::Print(m) => {
        //             assert_eq!(10, m[0]);
        //         }
        //         _ => assert!(false),
        //     }
        // } else {
        //     assert!(false);
        // }
    }

    bin_ops!(add_test, Operation::Add, +);
    bin_ops!(sub_test, Operation::Sub, -);
    bin_ops!(mul_test, Operation::Mul, *);
    bin_ops!(div_test, Operation::Div, /);
    bin_ops!(mod_test, Operation::Mod, %);

    #[test]
    #[ignore]
    fn reader_reads() {
        // let program = Program {
        //     trains: vec![Train {
        //         identifier: 0,
        //         config: TrainConfig {
        //             primary_color: ColorChoice::LightRed,
        //             secondary_color: ColorChoice::DarkRed,
        //             length: 1,
        //         },
        //         start: Target {
        //             span: Span::from_length(0, 1),
        //             track: 0,
        //             station: "Test".to_string(),
        //         },
        //         first_class_passengers: vec![],
        //         second_class_passengers: vec![],
        //     }],
        //     stations: vec![Station {
        //         name: "Test".to_string(),
        //         operation: Operation::Input,
        //         output: vec![Target {
        //             span: Span::from_length(0, 1),
        //             station: "Test".to_string(),
        //             track: 0,
        //         }],
        //     }],
        // };
        // let (sender, receiver) = channel();
        // let i = VMInterface::new();
        /*std::thread::spawn(|| {
            let pp = Data::new(program, i);
            pp.do_current_step(i).await.unwrap();
        });*/
        // while let Ok(t) = receiver.recv_timeout(Duration::from_millis(10)) {
        //     println!("{:?}", t);
        //     if let VmInterfaceMessage::AskForInput(n) = t {
        //         let h = i.message_ids_in_use.lock().await;
        //         let sender = h.get(&n).unwrap();
        //         sender.send(Input {
        //             value: vec![1, 2, 3, 4, 5],
        //         }).unwrap();
        //     }
        // }
    }

    #[tokio::test]
    async fn switch_switches() {
        let program = Program {
            trains: vec![Train {
                identifier: 0,
                config: TrainConfig {
                    primary_color: ColorChoice::LightRed,
                    secondary_color: ColorChoice::DarkRed,
                    length: 1,
                },
                start: Target {
                    span: Span::from_length(0, 1),
                    track: 0,
                    station: "Test".to_string(),
                },
                first_class_passengers: vec![],
                second_class_passengers: vec![SecondClassPassenger {
                    data: 10,
                    name: "Kees".to_string(),
                }],
            }],
            stations: vec![
                Station {
                    name: "Test".to_string(),
                    operation: Operation::SwitchGteZero,
                    output: vec![
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Other".to_string(),
                            track: 0,
                        },
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Other".to_string(),
                            track: 1,
                        },
                    ],
                },
                Station {
                    name: "Other".to_string(),
                    operation: Operation::SwitchEqZero,
                    output: vec![
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 0,
                        },
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 0,
                        },
                    ],
                },
            ],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        pp.do_current_step(i).await.unwrap();
        let other = pp.stations.get("Other").unwrap().lock().await;
        assert_eq!(other.trains[1].len(), 0);
        assert_eq!(other.trains[0].len(), 1);
        // while let Ok(t) = receiver.recv_timeout(Duration::from_millis(10)) {
        //     println!("{:?}", t);
        // }
    }

    #[tokio::test]
    async fn switch_empty() {
        let program = Program {
            trains: vec![Train {
                identifier: 0,
                config: TrainConfig {
                    primary_color: ColorChoice::LightRed,
                    secondary_color: ColorChoice::DarkRed,
                    length: 1,
                },
                start: Target {
                    span: Span::from_length(0, 1),
                    track: 0,
                    station: "Test".to_string(),
                },
                first_class_passengers: vec![],
                second_class_passengers: vec![],
            }],
            stations: vec![
                Station {
                    name: "Test".to_string(),
                    operation: Operation::SwitchEmpty,
                    output: vec![
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Other".to_string(),
                            track: 0,
                        },
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Other".to_string(),
                            track: 1,
                        },
                    ],
                },
                Station {
                    name: "Other".to_string(),
                    operation: Operation::SwitchEmpty,
                    output: vec![
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 0,
                        },
                        Target {
                            span: Span::from_length(0, 1),
                            station: "Test".to_string(),
                            track: 0,
                        },
                    ],
                },
            ],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        pp.do_current_step(i).await.unwrap();
        let other = pp.stations.get("Other").unwrap().lock().await;
        assert_eq!(other.trains[1].len(), 0);
        assert_eq!(other.trains[0].len(), 1);
        // while let Ok(t) = receiver.recv_timeout(Duration::from_millis(10)) {
        //     println!("{:?}", t);
        // }
    }

    #[tokio::test]
    async fn delete_train() {
        let program = Program {
            trains: vec![Train {
                identifier: 0,
                config: TrainConfig {
                    primary_color: ColorChoice::LightRed,
                    secondary_color: ColorChoice::DarkRed,
                    length: 1,
                },
                start: Target {
                    span: Span::from_length(0, 1),
                    track: 0,
                    station: "Test".to_string(),
                },
                first_class_passengers: vec![],
                second_class_passengers: vec![SecondClassPassenger {
                    data: 10,
                    name: "Kees".to_string(),
                }],
            }],
            stations: vec![Station {
                name: "Test".to_string(),
                operation: Operation::Delete,
                output: vec![
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test2".to_string(),
                        track: 0,
                    },
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test2".to_string(),
                        track: 1,
                    },
                ],
            }],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        assert_eq!(1, pp.train_count().await.unwrap());
        pp.do_current_step(i).await.unwrap();
        {
            let station = pp.stations.get("Test").unwrap().lock().await;
            assert_eq!(station.trains[0].len(), 0);
        }
        assert_eq!(0, pp.train_count().await.unwrap());
    }

    #[tokio::test]
    async fn del_top() {
        let program = Program {
            trains: vec![Train {
                identifier: 0,
                config: TrainConfig {
                    primary_color: ColorChoice::LightRed,
                    secondary_color: ColorChoice::DarkRed,
                    length: 1,
                },
                start: Target {
                    span: Span::from_length(0, 1),
                    track: 0,
                    station: "Test".to_string(),
                },
                first_class_passengers: vec![],
                second_class_passengers: vec![
                    SecondClassPassenger {
                        data: 10,
                        name: "Kees".to_string(),
                    },
                    SecondClassPassenger {
                        data: 20,
                        name: "Kees".to_string(),
                    },
                ],
            }],
            stations: vec![Station {
                name: "Test".to_string(),
                operation: Operation::DeleteTop,
                output: vec![
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test".to_string(),
                        track: 0,
                    },
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test".to_string(),
                        track: 1,
                    },
                ],
            }],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        pp.do_current_step(i).await.unwrap();
        let train = pp.trains.first().unwrap().lock().await;
        assert_eq!(1, train.train.second_class_passengers.len());
        assert_eq!(20, train.train.second_class_passengers.first().unwrap().data);
    }

    #[tokio::test]
    async fn duplicate() {
        let program = Program {
            trains: vec![Train {
                identifier: 0,
                config: TrainConfig {
                    primary_color: ColorChoice::LightRed,
                    secondary_color: ColorChoice::DarkRed,
                    length: 1,
                },
                start: Target {
                    span: Span::from_length(0, 1),
                    track: 0,
                    station: "Test".to_string(),
                },
                first_class_passengers: vec![],
                second_class_passengers: vec![SecondClassPassenger {
                    data: 10,
                    name: "Kees".to_string(),
                }],
            }],
            stations: vec![Station {
                name: "Test".to_string(),
                operation: Operation::Duplicate,
                output: vec![
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test2".to_string(),
                        track: 0,
                    },
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test2".to_string(),
                        track: 1,
                    },
                ],
            },
                           Station {
                               name: "Test2".to_string(),
                               operation: Operation::Duplicate,
                               output: vec![
                                   Target {
                                       span: Span::from_length(0, 1),
                                       station: "Test".to_string(),
                                       track: 0,
                                   },
                                   Target {
                                       span: Span::from_length(0, 1),
                                       station: "Test".to_string(),
                                       track: 1,
                                   },
                               ],
                           }],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        pp.do_current_step(i).await.unwrap();
        let station = pp.stations.get("Test2").unwrap().lock().await;
        assert_eq!(station.trains[0].len(), station.trains[1].len());
        let t1 = {
            station.trains[0]
                .front()
                .unwrap()
                .lock().await
                .train
                .clone()
        };
        assert_eq!(t1.second_class_passengers, station.trains[1].front().unwrap().lock().await.train.second_class_passengers);
        assert_ne!(t1.identifier, station.trains[1].front().unwrap().lock().await.train.identifier);
    }

    #[tokio::test]
    async fn banana_rotate() {
        let program = Program {
            trains: vec![Train {
                identifier: 0,
                config: TrainConfig {
                    primary_color: ColorChoice::LightRed,
                    secondary_color: ColorChoice::DarkRed,
                    length: 1,
                },
                start: Target {
                    span: Span::from_length(0, 1),
                    track: 0,
                    station: "Test".to_string(),
                },
                first_class_passengers: vec![],
                second_class_passengers: vec![
                    SecondClassPassenger {
                        data: 10,
                        name: "Kees1".to_string(),
                    },
                    SecondClassPassenger {
                        data: 20,
                        name: "Kees2".to_string(),
                    },
                    SecondClassPassenger {
                        data: 30,
                        name: "Kees3".to_string(),
                    },
                    SecondClassPassenger {
                        data: 40,
                        name: "Kees4".to_string(),
                    },
                ],
            }],
            stations: vec![Station {
                name: "Test".to_string(),
                operation: Operation::Rotate,
                output: vec![
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test".to_string(),
                        track: 0,
                    },
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test".to_string(),
                        track: 1,
                    },
                ],
            }],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        pp.do_current_step(i).await.unwrap();
        let train = pp.trains.first().unwrap().lock().await;
        assert_eq!(20, train.train.second_class_passengers[0].data);
    }

    #[tokio::test]
    async fn faster() {
        let program = Program {
            trains: vec![
                Train {
                    identifier: 0,
                    config: TrainConfig {
                        primary_color: ColorChoice::LightRed,
                        secondary_color: ColorChoice::DarkRed,
                        length: 1,
                    },
                    start: Target {
                        span: Span::from_length(0, 1),
                        track: 0,
                        station: "Test".to_string(),
                    },
                    first_class_passengers: vec![],
                    second_class_passengers: vec![
                        SecondClassPassenger {
                            data: 10,
                            name: "Kees1".to_string(),
                        },
                        SecondClassPassenger {
                            data: 20,
                            name: "Kees2".to_string(),
                        },
                        SecondClassPassenger {
                            data: 30,
                            name: "Kees3".to_string(),
                        },
                        SecondClassPassenger {
                            data: 40,
                            name: "Kees4".to_string(),
                        },
                    ],
                },
                Train {
                    identifier: 0,
                    config: TrainConfig {
                        primary_color: ColorChoice::LightRed,
                        secondary_color: ColorChoice::DarkRed,
                        length: 1,
                    },
                    start: Target {
                        span: Span::from_length(0, 1),
                        track: 1,
                        station: "Test".to_string(),
                    },
                    first_class_passengers: vec![],
                    second_class_passengers: vec![SecondClassPassenger {
                        data: 2000,
                        name: "Jan".to_string(),
                    }],
                },
            ],
            stations: vec![Station {
                name: "Test".to_string(),
                operation: Operation::Transfer,
                output: vec![
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test".to_string(),
                        track: 0,
                    },
                    Target {
                        span: Span::from_length(0, 1),
                        station: "Test".to_string(),
                        track: 1,
                    },
                ],
            }],
        };
        let i = Arc::new(VMInterface::new());
        let pp = Data::new(program).await;
        pp.do_current_step(i).await.unwrap();
        let train = pp.trains.first().unwrap().lock().await;
        assert_eq!(20, train.train.second_class_passengers[0].data);
        assert_eq!(3, train.train.second_class_passengers.len());
        let train2 = pp.trains.last().unwrap().lock().await;
        assert_eq!(10, train2.train.second_class_passengers[0].data);
        assert_eq!(2, train2.train.second_class_passengers.len());
    }
}
