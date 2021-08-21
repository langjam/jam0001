use std::path::PathBuf;
use structopt::StructOpt;

/// Available process arguments passed as command-line parameters.
#[derive(Debug, StructOpt)]
#[structopt(name = "parser", author)]
pub(crate) struct Args {
    /// Path to the input file.
    #[structopt(
        short = "f",
        long = "file",
        parse(from_os_str),
        name = "path to source file"
    )]
    pub(crate) path: PathBuf,

    /// Path to the build script to use;
    #[structopt(
        long = "build",
        short = "b",
        parse(from_os_str),
        name = "path to build script"
    )]
    pub(crate) build_script: PathBuf,
}

impl Args {
    pub(crate) fn read() -> Self {
        Args::from_args()
    }
}
