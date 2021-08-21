mod frontend;

use clap::{App, Arg};
use clap::crate_authors;
use crate::frontend::Communicator;
use std::path::Path;
use std::fs::File;
use std::io::Read;
use train::ast::Program;
use crate::frontend::web;

fn parse(v: &str) -> Program {todo!()}

#[tokio::main]
async fn main() {
    pretty_env_logger::env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .init();


    let matches = App::new("Train")
        .version("1.0")
        .author(crate_authors!("\n"))
        .about("Take a train to your destination do some computation along the way")
        .arg(Arg::with_name("program")
            .help("a program of your train network")
            .required(true)
        )
        .arg(Arg::with_name("cli")
            .long("cli")
            .short("c")
            .required(false)
            .takes_value(false)
            .help("a program of your train network"))
        .get_matches();


    let program_path = Path::new(matches.value_of("program").unwrap());
    let mut program_file = File::open(program_path).expect("file does not exist");
    let mut program = String::new();
    program_file.read_to_string(&mut program).expect("couldn't read");

    // let ast = parse(&program);

    let (comm, vmi) = Communicator::new();


    if matches.is_present("cli") {

    } else {
        web::run(comm, ).await;
    }



}
