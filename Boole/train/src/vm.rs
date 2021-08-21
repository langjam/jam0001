use crate::ast::{Program, Station, Train};
use crate::interface::VMInterface;
use crate::operations::Operation;
use std::collections::{HashMap, VecDeque};
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, Mutex};

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

impl DerefMut for StationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.station
    }
}

struct Data {
    stations: HashMap<String, Arc<Mutex<StationData>>>,
    trains: Vec<Arc<Mutex<TrainData>>>,
}

impl Data {
    fn new(program: Program) -> Data {
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
            st.trains[target.track].push_back(td);
        }
        Self { stations, trains }
    }

    fn do_current_step(&mut self, interface: &VMInterface) {
        for (name, station_arc) in self.stations.iter() {
            let mut station = station_arc.lock().unwrap();
            match station.operation {
                Operation::Nothing => {}
                Operation::Print => {
                    if let Some(x) = station.trains[0].front() {
                        let zz = x.lock().unwrap();
                        interface.print(
                            zz.train
                                .second_class_passengers
                                .iter()
                                .map(|x| x.data)
                                .collect(),
                        );
                    }
                }
                Operation::Input => {
                    if let Some(x) = station.trains[0].front() {
                        let mut t = x.lock().unwrap();
                        for (x, y) in t
                            .train
                            .second_class_passengers
                            .iter_mut()
                            .zip(interface.ask_for_input().unwrap_or(vec![]))
                        {
                            x.data = y;
                        }
                    }
                }
                Operation::Switch => {
                    if let Some(x) = station.trains[0].pop_front() {
                        let xcln = x.clone();
                        let st = x.lock().unwrap();
                        let val = st.train.second_class_passengers.first().unwrap();
                        if val.data > 0 {
                            let out = &station.output.clone()[0];
                            if out.station.eq(name) {
                                station.trains[out.track].push_back(xcln);
                            } else {
                                if let Some(station) = self.stations.get(out.station.as_str()) {
                                    let mut st = station.lock().unwrap();
                                    st.trains[out.track].push_back(xcln);
                                }
                            }
                        } else {
                            let out = &station.output.clone()[1];
                            if out.station.eq(name) {
                                station.trains[out.track].push_back(xcln);
                            } else {
                                if let Some(station) = self.stations.get(out.station.as_str()) {
                                    let mut st = station.lock().unwrap();
                                    st.trains[out.track].push_back(xcln);
                                }
                            }
                        }
                    }
                }
                Operation::Duplicate => {
                    if let Some(x) = station.trains[0].front().cloned() {
                        station.trains[1].push_back(x.clone());
                    }
                }
                Operation::Rotate => {
                    if let Some(x) = station.trains[0].front() {
                        let mut xx = x.lock().unwrap();
                        xx.train.second_class_passengers.rotate_right(1);
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
                        let target = &station.output.clone()[0];
                        if target.station == station.name {
                            station.trains[target.track].push_back(x);
                        } else {
                            if let Some(station) = self.stations.get(target.station.as_str()) {
                                let mut st = station.lock().unwrap();
                                st.trains[target.track].push_back(x);
                            }
                        }
                    }
                }

                {
                    if let Some(x) = station.trains[1].pop_front() {
                        let target = &station.output.clone()[1];
                        if target.station == station.name {
                            station.trains[target.track].push_back(x)
                        } else {
                            if let Some(station) = self.stations.get(target.station.as_str()) {
                                let mut st = station.lock().unwrap();
                                st.trains[target.track].push_back(x);
                            }
                        }
                    }
                }
            }
        }
        interface.end_simulation_step();
    }

    // help
    /*fn do_current_step(&mut self, interface: &VMInterface) {
        let mut del_idxes = vec![];
        let mut should_append = vec![];
        for (i, train_arc) in self.trains.iter().enumerate() {
            let mut train = train_arc.lock().unwrap();
            let st = self
                .stations
                .get_mut(train.clone().station_name.as_str())
                .unwrap();
            let other_train = st.trains[1 - train.track].front();
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
                        st.trains[1].push_back(train_arc.clone());
                        should_append.push(train.clone());
                    }
                }
                Operation::Rotate => {
                    if train.track == 0 {
                        match other_train {
                            None => train.train.second_class_passengers.rotate_right(1),
                            Some(x) => train.train.second_class_passengers.rotate_right(
                                x.lock().unwrap().first_passenger_value().unwrap()
                                    as usize,
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

                        let mut pass = other_train.lock().unwrap().train.second_class_passengers;
                        pass.push(f);
                        pass.rotate_right(1);


                    }
                }
                Operation::Add => {
                    if train.track == 0 {
                        match other_train {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data += x
                                    .lock()
                                    .unwrap()
                                    .train
                                    .second_class_passengers
                                    .get(0)
                                    .unwrap()
                                    .data
                            }
                        }
                    }
                }
                Operation::Sub => {
                    if train.track == 0 {
                        match other_train {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data -= x
                                    .lock()
                                    .unwrap()
                                    .train
                                    .second_class_passengers
                                    .get(0)
                                    .unwrap()
                                    .data
                            }
                        }
                    }
                }
                Operation::Mul => {
                    if train.track == 0 {
                        match other_train {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data *= x
                                    .lock()
                                    .unwrap()
                                    .train
                                    .second_class_passengers
                                    .get(0)
                                    .unwrap()
                                    .data
                            }
                        }
                    }
                }
                Operation::Div => {
                    if train.track == 0 {
                        match other_train {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data /= x
                                    .lock()
                                    .unwrap()
                                    .train
                                    .second_class_passengers
                                    .get(0)
                                    .unwrap()
                                    .data
                            }
                        }
                    }
                }
                Operation::Mod => {
                    if train.track == 0 {
                        match other_train {
                            None => {}
                            Some(x) => {
                                train.train.second_class_passengers.get_mut(0).unwrap().data %= x
                                    .lock()
                                    .unwrap()
                                    .train
                                    .second_class_passengers
                                    .get(0)
                                    .unwrap()
                                    .data
                            }
                        }
                    }
                }
                Operation::Delete => {
                    del_idxes.push(i);
                }
            }
        }
    }*/
}

#[cfg(test)]
mod tests {
    use crate::ast::{Program, SecondClassPassenger, Station, Target, Train};
    use crate::operations::Operation;
    use crate::vm::Data;
    use crate::interface::VMInterface;

    #[test]
    fn stupid_stuff() {
        // let program = Program {
        //     trains: vec![Train {
        //         start: Target {
        //             track: 0,
        //             station: "Test".to_string(),
        //         },
        //         first_class_passengers: vec![],
        //         second_class_passengers: vec![SecondClassPassenger {
        //             data: 10,
        //             name: "Kees".to_string(),
        //         }],
        //     }],
        //     stations: vec![Station {
        //         name: "Test".to_string(),
        //         operation: Operation::Nothing,
        //         output: vec![Target {
        //             station: "Test".to_string(),
        //             track: 0,
        //         }],
        //     }],
        // };
        // let mut pp = Data::new(program);
        // // pp.do_current_step(&VMInterface {})
    }
}
