
#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum Operation {
    Nothing,
    Print,
    Input,
    Switch,

    Duplicate,
    Rotate,
    DeleteTop,
    Transfer,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Delete,
}

impl Operation {
    pub fn input_track_count(&self) -> usize {
        match self {
            Operation::Nothing => 1,
            Operation::Print => 1,
            Operation::Input => 0,
            Operation::Switch => 1,

            Operation::Duplicate => 1,
            Operation::Rotate => 1,
            Operation::DeleteTop => 1,
            Operation::Transfer => 2,

            Operation::Add => 2,
            Operation::Sub => 2,
            Operation::Mul => 2,
            Operation::Div => 2,
            Operation::Mod => 2,

            Operation::Delete => 1,
        }
    }
}