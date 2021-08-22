use serde::Serialize;


/// The various operations our stations can do.
///
/// * [Nothing](Operation::Nothing) Do nothing
///
/// Character manipulation
/// * [PrintString](Operation::PrintString) prints the current second class data as if it is a UTF-8 string.
/// * [PrintNumber](Operation::PrintNumber) prints the current second class data as raw numbers.
/// * [Input](Operation::Input) reads input, and stores it as bytes into the second class.
///
/// If statements
/// * [SwitchEqZero](Operation::SwitchEqZero) goes to track 1 if the train's current leading passenger has the value 0 attached to them, and picks track 2 otherwise.
/// * [SwitchGteZero](Operation::SwitchGteZero) goes to track 1 if passenger number 0 has a positive number attached to them.
/// * [SwitchEmpty](Operation::SwitchEmpty) if the train has no passengers, goes to track 1, and else track 2.
///
///
/// Train specific operators
///
/// * [Duplicate](Operation::Duplicate) duplicates the train from track 1 to track 2.
/// * [Rotate](Operation::Rotate) rotates the first second class passenger to the back.
/// * [DeleteTop](Operation::DeleteTop) makes the first second class passenger exit the train at the current station.
/// * [Transfer](Operation::Transfer) transfers the first second class passenger of track 1 to the train in track 2.
///
/// Math operators
/// These all work roughly the same, the value of track 2 is `operator`'d to the value of track 1.
/// * [Add](Operation::Add) does a `+`
/// * [Sub](Operation::Sub) does a `-`
/// * [Mul](Operation::Mul) does a `*`
/// * [Div](Operation::Div) does a `/`
/// * [Mod](Operation::Mod) does a `%`
///
/// Delete
/// * [Delete](Operation::Delete) removes the train from existence.
#[derive(EnumIter, Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub enum Operation {
    #[serde(rename="nothing")]
    Nothing,

    #[serde(rename="print_string")]
    PrintString,
    #[serde(rename="print_number")]
    PrintNumber,
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
            Operation::Input => 1,
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