mod frontend;

use clap::{App, Arg};
use clap::crate_authors;
use std::path::Path;
use std::fs::File;
use std::io::Read;
use crate::frontend::web;
use train::parse_and_check;
use train::vm::Data;
use crate::frontend::cli::CliRunner;

#[tokio::main(flavor = "multi_thread", worker_threads = 5)]
async fn main() {
    pretty_env_logger::env_logger::builder()
        .filter_level(log::LevelFilter::Info)
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

    log::info!("running: {:?}", program_path);

    let mut program_file = File::open(program_path).expect("file does not exist");
    let mut program = String::new();
    program_file.read_to_string(&mut program).expect("couldn't read");

    let ast = match parse_and_check(&program) {
        Ok(program) => program,
        Err(err) => {
            log::error!("{}", err);
            return;
        }
    };

    if matches.is_present("cli") {
        let runner = CliRunner::new();
        let vm = Data::new(ast);
        runner.run(vm);
    } else {
        web::run(ast).await;
    }
}
