use crate::operations::Operation;

pub struct Program {
    pub trains: Vec<Train>,
    pub stations: Vec<Station>,
}

pub struct Train {
    pub start: Target,
    pub first_class_passengers: Vec<FirstClassPassenger>,
    pub second_class_passengers: Vec<SecondClassPassenger>
}

pub struct FirstClassPassenger {
    pub name: String,
    pub data: String,
}

pub struct SecondClassPassenger {
    pub name: String,
    pub data: i64
}

pub struct Station {
    pub name: String,
    pub operation: Operation,
    pub output: Vec<Target>
}

pub struct Target {
    pub station: String,
    pub track: usize,
}