
#[derive(EnumIter, Debug, Copy, Clone, Eq, PartialEq)]
pub enum Operation {
    Nothing,
    PrintString,
    PrintNumber,
    Input,
    SwitchEqZero,
    SwitchGteZero,
    SwitchEmpty,

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
    pub fn name(&self) -> &'static str {
        match self {
            Operation::Nothing => "nothing",
            Operation::PrintString => "print string",
            Operation::PrintNumber => "print number",
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
            Operation::PrintString => 1,
            Operation::PrintNumber => 1,
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