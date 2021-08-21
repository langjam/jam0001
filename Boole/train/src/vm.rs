use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use crate::ast::{Program, Station, Train};
use crate::interface::VMInterface;
use crate::operations::Operation;

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

struct Data {
    stations: HashMap<String, Arc<Mutex<StationData>>>,
    trains: Vec<Arc<Mutex<TrainData>>>,
}

impl Data {
    fn new(program: Program, interface: &VMInterface) -> Data {
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
                .unwrap();
            interface.train_to_start(st.station.clone(), td.lock().unwrap().train.clone()).unwrap_or(());
            st.trains[target.track].push_back(td);
        }
        Self { stations, trains }
    }

    fn do_current_step(&mut self, interface: &VMInterface) {
        let mut targets = vec![];
        for (_, station_arc) in self.stations.iter() {
            let mut station = station_arc.lock().unwrap();
            match station.operation {
                Operation::Nothing => {}
                Operation::Print => {
                    if let Some(x) = station.trains[0].front() {
                        let zz = x.lock().unwrap();
                        if let Ok(_) = interface.print(
                            zz.train
                                .second_class_passengers
                                .iter()
                                .map(|x| x.data)
                                .collect(),
                        ) {
                        } else {
                            return;
                        }
                    }
                }
                Operation::Input => {
                    if let Some(x) = station.trains[0].front() {
                        let mut t = x.lock().unwrap();
                        if let Ok(other) = interface.ask_for_input() {
                            for (x, y) in t.train.second_class_passengers.iter_mut().zip(other) {
                                x.data = y;
                            }
                        } else {
                            return;
                        }
                    }
                }
                Operation::Switch => {
                    if let Some(x) = station.trains[0].pop_front() {
                        let xcln = x.clone();
                        let st = x.lock().unwrap();
                        let val = st.train.second_class_passengers.first().unwrap();
                        if val.data > 0 {
                            let target = station.output.clone()[0].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                        } else {
                            let target = station.output.clone()[1].clone();
                            targets.push((xcln, target, station.station.clone(), 0));
                        }
                    }
                }
                Operation::Duplicate => {
                    if let Some(x) = station.trains[0].front().cloned() {
                        station.trains[1].push_back(x);
                    }
                }
                Operation::Rotate => {
                    if let Some(x) = station.trains[0].front() {
                        let mut xx = x.lock().unwrap();
                        xx.train.second_class_passengers.rotate_left(1);
                    }
                }
                Operation::DeleteTop => {
                    if let Some(x) = station.trains[0].front() {
                        let mut xx = x.lock().unwrap();
                        xx.train.second_class_passengers.remove(0);
                    }
                }
                Operation::Transfer => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let xx = {
                                let mut z = x.lock().unwrap();
                                z.train.second_class_passengers.remove(0)
                            };
                            let mut yy = y.lock().unwrap();
                            yy.train.second_class_passengers.push(xx);
                            yy.train.second_class_passengers.rotate_right(1);
                        }
                    }
                }
                Operation::Add => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().unwrap();
                            let yy = y.lock().unwrap();
                            let mut first_data =
                                xx.train.second_class_passengers.get_mut(0).unwrap();
                            let second_data =
                                yy.train.second_class_passengers.first().unwrap().data;
                            first_data.data += second_data;
                        }
                    }
                }
                Operation::Sub => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().unwrap();
                            let yy = y.lock().unwrap();
                            let mut first_data =
                                xx.train.second_class_passengers.get_mut(0).unwrap();
                            let second_data =
                                yy.train.second_class_passengers.first().unwrap().data;
                            first_data.data -= second_data;
                        }
                    }
                }
                Operation::Mul => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().unwrap();
                            let yy = y.lock().unwrap();
                            let mut first_data =
                                xx.train.second_class_passengers.get_mut(0).unwrap();
                            let second_data =
                                yy.train.second_class_passengers.first().unwrap().data;
                            first_data.data *= second_data;
                        }
                    }
                }
                Operation::Div => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().unwrap();
                            let yy = y.lock().unwrap();
                            let mut first_data =
                                xx.train.second_class_passengers.get_mut(0).unwrap();
                            let second_data =
                                yy.train.second_class_passengers.first().unwrap().data;
                            first_data.data /= second_data;
                        }
                    }
                }
                Operation::Mod => {
                    if let Some(x) = station.trains[0].front() {
                        if let Some(y) = station.trains[1].front() {
                            let mut xx = x.lock().unwrap();
                            let yy = y.lock().unwrap();
                            let mut first_data =
                                xx.train.second_class_passengers.get_mut(0).unwrap();
                            let second_data =
                                yy.train.second_class_passengers.first().unwrap().data;
                            first_data.data %= second_data;
                        }
                    }
                }
                Operation::Delete => {
                    let st = &mut station.trains[0];
                    st.pop_front();
                }
            }

            if station.operation != Operation::Delete && station.operation != Operation::Switch {
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
                .unwrap()
                .lock()
                .unwrap();
            {
                let tr = train.lock().unwrap();
                interface.move_train(
                    current,
                    station.station.clone(),
                    tr.train.clone(),
                    track,
                    target.track,
                ).unwrap_or(());
            }
            station.trains[target.track].push_back(train);
        }
        interface.end_simulation_step().unwrap_or(());
    }
}

#[cfg(test)]
mod tests {
    use std::sync::mpsc::channel;

    use crate::ast::{Program, SecondClassPassenger, Station, Target, Train};
    use crate::interface::{VMInterface, VmInterfaceMessage};
    use crate::operations::Operation;
    use crate::parse::parser::Span;
    use crate::vm::Data;
    use std::time::Duration;

    macro_rules! create_program {
        ($x:expr) => {
            Program {
                trains: vec![
                    Train {
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
            #[test]
            fn $func() {
                let program = create_program!($x);
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
                pp.do_current_step(&i);
                let train = pp.trains[0].lock().unwrap();
                assert_eq!(Some(10 $op 20), train.first_passenger_value());
            }
        };
    }

    #[test]
    fn nothing_does_nothing() {
        let program = create_program!(Operation::Nothing);
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        let train = pp.trains[0].lock().unwrap();
        assert_eq!(Some(10), train.first_passenger_value());
    }

    #[test]
    fn trains_travel() {
        let program = Program {
            trains: vec![Train {
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
        let (sender, receiver) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        {
            let station = pp.stations.get("Test2").unwrap().lock().unwrap();
            assert_eq!(station.trains[0].len(), 1);
        }
        pp.do_current_step(&i);
        {
            let station = pp.stations.get("Test").unwrap().lock().unwrap();
            assert_eq!(station.trains[1].len(), 1);
        }
        pp.do_current_step(&i);
        {
            let station = pp.stations.get("Test2").unwrap().lock().unwrap();
            assert_eq!(station.trains[1].len(), 1);
        }
        pp.do_current_step(&i);
        {
            let station = pp.stations.get("Test").unwrap().lock().unwrap();
            assert_eq!(station.trains[0].len(), 1);
        }
        while let Ok(t) = receiver.recv_timeout(Duration::from_millis(10)) {
            println!("{:?}", t);
        }
    }

    #[test]
    fn print_prints() {
        let program = create_program!(Operation::Print);
        let (sender, receiver) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        receiver.recv().unwrap();
        receiver.recv().unwrap();
        if let Ok(m) = receiver.recv() {
            match m {
                VmInterfaceMessage::Print(m) => {
                    assert_eq!(10, m[0]);
                }
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }
    }

    bin_ops!(add_test, Operation::Add, +);
    bin_ops!(sub_test, Operation::Sub, -);
    bin_ops!(mul_test, Operation::Mul, *);
    bin_ops!(div_test, Operation::Div, /);
    bin_ops!(mod_test, Operation::Mod, %);

    // #[test]
    fn reader_reads() {
        let program = create_program!(Operation::Input);
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
    }

    #[test]
    fn switch_switches() {
        let program = Program {
            trains: vec![Train {
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
                    operation: Operation::Switch,
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
                    operation: Operation::Switch,
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
        let (sender, receiver) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        let other = pp.stations.get("Other").unwrap().lock().unwrap();
        assert_eq!(other.trains[1].len(), 0);
        assert_eq!(other.trains[0].len(), 1);
        while let Ok(t) = receiver.recv_timeout(Duration::from_millis(10)) {
            println!("{:?}", t);
        }
    }

    #[test]
    fn delete_train() {
        let program = Program {
            trains: vec![Train {
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
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        let station = pp.stations.get("Test").unwrap().lock().unwrap();
        assert_eq!(station.trains[0].len(), 0)
    }

    #[test]
    fn del_top() {
        let program = Program {
            trains: vec![Train {
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
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        let train = pp.trains.first().unwrap().lock().unwrap();
        assert_eq!(0, train.train.second_class_passengers.len());
    }

    #[test]
    fn duplicate() {
        let program = Program {
            trains: vec![Train {
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
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        let station = pp.stations.get("Test").unwrap().lock().unwrap();
        assert_eq!(station.trains[0].len(), station.trains[1].len());
        let t1 = {
            station.trains[0]
                .front()
                .unwrap()
                .lock()
                .unwrap()
                .train
                .clone()
        };
        assert_eq!(t1, station.trains[1].front().unwrap().lock().unwrap().train);
    }

    #[test]
    fn banana_rotate() {
        let program = Program {
            trains: vec![Train {
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
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        let train = pp.trains.first().unwrap().lock().unwrap();
        assert_eq!(20, train.train.second_class_passengers[0].data);
    }

    #[test]
    fn faster() {
        let program = Program {
            trains: vec![
                Train {
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
        let (sender, _) = channel();
        let i = VMInterface::new(sender);
        let mut pp = Data::new(program, &i);
        pp.do_current_step(&i);
        let train = pp.trains.first().unwrap().lock().unwrap();
        assert_eq!(20, train.train.second_class_passengers[0].data);
        assert_eq!(3, train.train.second_class_passengers.len());
        let train2 = pp.trains.last().unwrap().lock().unwrap();
        assert_eq!(10, train2.train.second_class_passengers[0].data);
        assert_eq!(2, train2.train.second_class_passengers.len());
    }
}
