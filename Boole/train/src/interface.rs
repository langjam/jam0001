use crate::ast::Station;

pub struct VMInterface {
    // sender: Sender<SimulationUpdate>
}

impl VMInterface {
    pub fn new() {

    }

    pub fn ask_for_input(&self) -> Vec<i64> {
        todo!()
    }
    pub fn print(&self, data: Vec<i64>) { todo!() }
    pub fn move_train(&self, from_station: &Station, to_station: &Station) { todo!() }
    pub fn end_simulation_step(&self) { todo!() }
}