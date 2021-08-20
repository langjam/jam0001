use crate::ast::{Program, Station, Target, Train};
use crate::interface::VMInterface;
use crate::operations::Operation;
use std::collections::{HashMap, VecDeque};
use std::ops::{Deref, DerefMut};

#[derive(Clone)]
struct TrainData {
    train: Train,
    station_name: String,
    track: usize,
}

impl TrainData {
    fn new(train: Train, mut sd: &mut StationData) -> Self {
        let st = train.start.station.clone();
        let tr = train.start.track.clone();
        let td = Self {
            train: train.clone(),
            station_name: st.clone(),
            track: tr.clone(),
        };
        sd.trains[tr.clone()].push_back(td.clone());
        td
    }
}

#[derive(Clone)]
pub struct StationData {
    trains: [VecDeque<TrainData>; 2],
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

impl DerefMut for StationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.station
    }
}

#[derive(Clone)]
struct Data {
    stations: HashMap<String, StationData>,
    trains: Vec<TrainData>,
}

impl Data {
    fn new(program: Program) -> Data {
        let mut stations = HashMap::new();
        for station in program.stations {
            stations.insert(station.clone().name, StationData::new(station));
        }
        let mut trains = vec![];
        for train in program.trains {
            trains.push(TrainData::new(
                train.clone(),
                stations.get_mut(train.start.station.as_str()).unwrap(),
            ))
        }
        Self { stations, trains }
    }

    /// help
    fn do_current_step(&mut self, interface: &VMInterface) {
        let mut del_idxes = vec![];
        let mut should_append = vec![];
        for (i, mut train) in self.trains.iter_mut().enumerate() {
            let st = self
                .stations
                .get_mut(train.clone().station_name.as_str())
                .unwrap();
            let mut target = st.clone().output.get(train.track).unwrap();
            match st.operation {
                Operation::Nothing => {}
                Operation::Print => interface.print(
                    train
                        .train
                        .second_class_passengers
                        .iter()
                        .map(|x| x.data)
                        .collect(),
                ),
                Operation::Input => {
                    let input = interface.ask_for_input();
                    for (i, second_class_passenger) in
                        train.train.second_class_passengers.iter_mut().enumerate()
                    {
                        second_class_passenger.data = input[i];
                    }
                }
                Operation::Switch => {
                    if train.train.second_class_passengers.first().unwrap().data > 1 {
                        target = &st.output[0]
                    } else {
                        target = &st.output[1]
                    }
                }
                Operation::Duplicate => {
                    if train.track == 0 {
                        st.trains[1].push_back(train.clone());
                        should_append.push(train.clone());
                    }
                }
                Operation::Rotate => {
                    if train.track == 0 {
                        if st.trains[1].is_empty() {
                            train.train.second_class_passengers.rotate_right(1);
                        } else {
                            train.train.second_class_passengers.rotate_right(
                                st.trains[2]
                                    .front()
                                    .unwrap()
                                    .train
                                    .second_class_passengers
                                    .first()
                                    .unwrap()
                                    .data as usize,
                            )
                        }
                    }
                }
                Operation::DeleteTop => {
                    train.train.second_class_passengers.remove(0);
                }
                Operation::Transfer => {
                    if train.track == 0 {
                        let f = train.train.second_class_passengers.first().unwrap().clone();
                        train.train.second_class_passengers.remove(0);
                        st.trains[1]
                            .front_mut()
                            .unwrap()
                            .train
                            .second_class_passengers
                            .push(f);
                        st.trains[1]
                            .front_mut()
                            .unwrap()
                            .train
                            .second_class_passengers
                            .rotate_right(1)
                    }
                }
                Operation::Add => {
                    if train.track == 0 {
                        match st.trains[1].clone().front() {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data +=
                                    x.train.second_class_passengers.get(0).unwrap().data
                            }
                        }
                    }
                }
                Operation::Sub => {
                    if train.track == 0 {
                        match st.trains[1].clone().front() {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data -=
                                    x.train.second_class_passengers.get(0).unwrap().data
                            }
                        }
                    }
                }
                Operation::Mul => {
                    if train.track == 0 {
                        match st.trains[1].clone().front() {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data *=
                                    x.train.second_class_passengers.get(0).unwrap().data
                            }
                        }
                    }
                }
                Operation::Div => {
                    if train.track == 0 {
                        match st.trains[1].clone().front() {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data /=
                                    x.train.second_class_passengers.get(0).unwrap().data
                            }
                        }
                    }
                }
                Operation::Mod => {
                    if train.track == 0 {
                        match st.trains[1].clone().front() {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data %=
                                    x.train.second_class_passengers.get(0).unwrap().data
                            }
                        }
                    }
                }
                Operation::Delete => {
                    del_idxes.push(i);
                }
            }

            if st.operation != Operation::Delete {
                let target = st.output[train.track].clone();
                let new_st = self.stations.get_mut(target.station.as_str()).unwrap();
                new_st.trains[target.track].push_back(train.clone());
            }
        }
        del_idxes.sort();
        del_idxes.reverse();
        for del_idx in del_idxes {
            let tr = self.trains.remove(del_idx);
        }


        self.trains.append(&mut should_append);
    }
}
