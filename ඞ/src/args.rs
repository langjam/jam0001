use std::path::PathBuf;
use structopt::StructOpt;

/// Available process arguments passed as command-line parameters.
#[derive(Debug, StructOpt)]
#[structopt(name = "parser", author)]
pub(crate) struct Args {
    /// Path to the build script to use.
    #[structopt(
        long = "build",
        short = "b",
        parse(from_os_str),
        name = "path to build script"
    )]
    pub(crate) build_script: PathBuf,

    /// Args passed to build script
    #[structopt(name = "build arguments")]
    pub(crate) args: Vec<String>,
}

impl Args {
    pub(crate) fn read() -> Self {
        Args::from_args()
    }
}
