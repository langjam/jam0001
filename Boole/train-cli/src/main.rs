use clap::{App, Arg};
use clap::crate_authors;

fn main() {
    let matches = App::new("Train")
        .version("1.0")
        .author(crate_authors!("\n"))
        .about("Take a train to your destination do some computation along the way")
        .arg(Arg::with_name("program")
            .help("a program of your train network"))
        .get_matches();


}
