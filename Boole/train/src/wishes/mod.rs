use color::Rgb;

pub mod parse_wishes;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TrainConfig {
    pub(crate) primary_color: Rgb,
    pub(crate) secondary_color: Rgb,
    pub(crate) length: usize,
}

#[derive(EnumIter, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ColorChoice {
    LightRed, //rgb(204, 68, 82)
    WaterBlue, //rgb(36, 97, 128)
    DarkRed, //rgb(128, 29, 39)
    LightBlue, //rgb(47, 152, 204)
    DarkGreen, //rgb(17, 128, 42)
    LightGreen, //rgb(67, 204, 98)
    LimeGreen, //rgb(57, 204, 174)
    Brown, //rgb(102, 82, 74)
    BrownGreen, //rgb(128, 124, 23)
    OrangeRed, //rgb(204, 111, 78)
}

impl ColorChoice {
    pub fn name(&self) -> &'static str {
        match self {
            ColorChoice::LightRed => "light red",
            ColorChoice::WaterBlue => "water blue",
            ColorChoice::DarkRed => "dark red",
            ColorChoice::LightBlue => "light blue",
            ColorChoice::DarkGreen => "dark green",
            ColorChoice::LightGreen => "light green",
            ColorChoice::LimeGreen => "lime green",
            ColorChoice::Brown => "brown",
            ColorChoice::BrownGreen => "brown green",
            ColorChoice::OrangeRed => "orange red",
        }
    }

    pub fn color(&self) -> Rgb {
        match self {
            ColorChoice::LightRed => Rgb::new(204, 68, 82 ),
            ColorChoice::WaterBlue => Rgb::new( 36, 97, 128 ),
            ColorChoice::DarkRed => Rgb::new( 128, 29, 39 ),
            ColorChoice::LightBlue => Rgb::new( 47, 152, 204 ),
            ColorChoice::DarkGreen => Rgb::new( 17, 128, 42 ),
            ColorChoice::LightGreen => Rgb::new( 67, 204, 98 ),
            ColorChoice::LimeGreen => Rgb::new( 57, 204, 174 ),
            ColorChoice::Brown => Rgb::new( 102, 82, 74 ),
            ColorChoice::BrownGreen => Rgb::new( 128, 124, 23 ),
            ColorChoice::OrangeRed => Rgb::new( 204, 111, 78 ),
        }
    }
}