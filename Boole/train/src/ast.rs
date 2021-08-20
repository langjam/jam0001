use crate::operations::Operation;

pub struct Program {
    trains: Vec<Train>,
    stations: Vec<Station>,
}

pub struct Train {
    start: Target,
    first_class_passengers: Vec<FirstClassPassenger>,
    second_class_passengers: Vec<SecondClassPassenger>
}

pub struct FirstClassPassenger {
    name: String,
    data: String,
}

pub struct SecondClassPassenger {
    name: String,
    data: i64
}

pub struct Station {
    name: String,
    operation: Operation,
    output: Vec<Target>
}

pub struct Target {
    station: String,
    track: usize,
}