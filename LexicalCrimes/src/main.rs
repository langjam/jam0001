#![allow(dead_code)]
use std::{env, process};

// core code
mod core;
// standard library
mod lib;

use crate::{
    core::junction,
    core::messages::*,
};

fn main() {
    // get the script filename from the command-line
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let script_path = &args[1];

        // start control flow
        junction::execute(script_path)
    } else {
        push_help();
        push_error("Script path not provided.".to_string());
        process::exit(0)
    }
}