use crate::ast::FirstClassPassenger;
use crate::wishes::*;
use strum::*;
use rand::Rng;

pub fn parse_wishes(input: &Vec<FirstClassPassenger>) -> TrainConfig {
    let mut primary_colors = vec![];
    let mut secondary_colors = vec![];
    let mut lengths = vec![];

    for passenger in input {
        let text = passenger.data.to_lowercase();
        for color in ColorChoice::iter() {
            if text.contains(&format!("{} train", color.name()))
                || text.contains(&format!("{} color", color.name()))
                || text.contains(&format!("{} colour", color.name()))
                || text.contains(&format!("{} locomotive", color.name())){
                primary_colors.push(color);
            }
            if text.contains(&format!("{} stripe", color.name()))
                || text.contains(&format!("{} line", color.name()))
                || text.contains(&format!("{} accent", color.name()))
                || text.contains(&format!("{} detail", color.name())){
                secondary_colors.push(color);
            }
        }
        if text.contains("long") || text.contains("big") {
            lengths.push(3);
        } else if text.contains("medium") || text.contains("intermediate") || text.contains("normal") {
            lengths.push(2);
        } else if  text.contains("short") ||  text.contains("small") ||  text.contains("compact") {
            lengths.push(1);
        }
    }

    let primary_color = combine_colors(primary_colors);
    let secondary_color = combine_colors(secondary_colors);
    let length = if lengths.len() > 0 {
        lengths.iter().sum::<usize>() / lengths.len()
    } else {
        2
    };
    TrainConfig{ primary_color, secondary_color, length }
}

fn combine_colors(input: Vec<ColorChoice>) -> ColorChoice {
    let mut rng = rand::thread_rng();
    if input.len() == 0 {
        let colors: Vec<ColorChoice> = ColorChoice::iter().collect();
        return colors[rng.gen_range(0..colors.len())]
    }

    return input[rng.gen_range(0..input.len())]
}