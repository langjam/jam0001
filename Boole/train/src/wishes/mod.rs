pub mod parse_wishes;
use serde::Serialize;

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct TrainConfig {
    pub(crate) primary_color: ColorChoice,
    pub(crate) secondary_color: ColorChoice,
    pub(crate) length: usize,
}

#[derive(EnumIter, Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub enum ColorChoice {
    LightRed, //rgb(204, 68, 82)
    DarkBlue, //rgb(36, 97, 128)
    DarkRed, //rgb(128, 29, 39)
    LightBlue, //rgb(47, 152, 204)
    DarkGreen, //rgb(17, 128, 42)
    LightGreen, //rgb(67, 204, 98)
    WaterBlue, //rgb(57, 204, 174)
    Brown, //rgb(102, 82, 74)
    Yellow, //rgb(128, 124, 23)
    Orange, //rgb(204, 111, 78)
}

impl ColorChoice {
    pub fn name(&self) -> &'static str {
        match self {
            ColorChoice::LightRed => "light red",
            ColorChoice::DarkBlue => "dark blue",
            ColorChoice::DarkRed => "dark red",
            ColorChoice::LightBlue => "light blue",
            ColorChoice::DarkGreen => "dark green",
            ColorChoice::LightGreen => "light green",
            ColorChoice::WaterBlue => "water blue",
            ColorChoice::Brown => "brown",
            ColorChoice::Yellow => "yellow",
            ColorChoice::Orange => "orange",
        }
    }
}