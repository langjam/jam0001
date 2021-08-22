#![allow(unused_doc_comments)]

use eyre::WrapErr;
use std::{
    fs::File,
    io::{BufReader, Read},
};

#[comment_errors::wrap]
fn get_config() -> eyre::Result<String> {
    let config_path = "doesn't exist";

    /// # Reading config file
    ///
    let file = {
        // We can refer to variables from scope in error messages!

        /// ## Opening file: `<config_path>`
        ///
        /// It **failed**, for one of the following reasons:
        ///
        /// * You don't have *permissions* to read this file.
        /// * It hasn't been created yet.
        File::open(config_path)?
    };
    let mut reader = BufReader::new(file);
    let mut buffer = String::new();

    /// # Loading config
    ///
    /// There could be a *decoding* error?
    reader.read_to_string(&mut buffer)?;

    Ok(buffer)
}

#[comment_errors::wrap]
fn main() -> eyre::Result<()> {
    /// # Getting config
    ///
    /// This is error prone as it attempts to read a config from your file system.
    /// If you *hit* this, please **[submit an issue](https://github.com/langjam/jam0001/issues/new)**
    let config = get_config()?;
    println!("{}", config);
    Ok(())
}
