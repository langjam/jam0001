use serde::Serialize;


#[derive(EnumIter, Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub enum Operation {
    #[serde(rename="nothing")]
    Nothing,
    #[serde(rename="print")]
    Print,
    #[serde(rename="input")]
    Input,
    #[serde(rename="switch_eqz")]
    SwitchEqZero,
    #[serde(rename="switch_gtez")]
    SwitchGteZero,
    #[serde(rename="switch_empty")]
    SwitchEmpty,

    #[serde(rename="duplicate")]
    Duplicate,
    #[serde(rename="rotate")]
    Rotate,
    #[serde(rename="delete_top")]
    DeleteTop,
    #[serde(rename="transfer")]
    Transfer,

    #[serde(rename="add")]
    Add,
    #[serde(rename="sub")]
    Sub,
    #[serde(rename="mul")]
    Mul,
    #[serde(rename="div")]
    Div,
    #[serde(rename="mod")]
    Mod,

    #[serde(rename="delete")]
    Delete,
}

impl Operation {
    pub fn name(&self) -> &'static str {
        match self {
            Operation::Nothing => "nothing",
            Operation::Print => "print",
            Operation::Input => "input",
            Operation::SwitchEqZero => "switch eq",
            Operation::SwitchGteZero => "switch gte",
            Operation::SwitchEmpty => "switch empty",
            Operation::Duplicate => "duplicate",
            Operation::Rotate => "rotate",
            Operation::DeleteTop => "delete top",
            Operation::Transfer => "transfer",
            Operation::Add => "add",
            Operation::Sub => "sub",
            Operation::Mul => "mul",
            Operation::Div => "div",
            Operation::Mod => "mod",
            Operation::Delete => "delete",
        }
    }

    pub fn input_track_count(&self) -> usize {
        match self {
            Operation::Nothing => 1,
            Operation::Print => 1,
            Operation::Input => 0,
            Operation::SwitchEqZero => 1,
            Operation::SwitchGteZero => 1,
            Operation::SwitchEmpty => 1,

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