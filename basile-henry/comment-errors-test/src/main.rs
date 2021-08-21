#![allow(unused_doc_comments)]

use eyre::WrapErr;
use std::{
    fs::File,
    io::{BufReader, Read},
};

#[comment_errors::wrap]
fn main() -> eyre::Result<()> {
    let file = {
        /// # Custom Error Title
        ///
        /// The file you are trying to *open* doesn't **exist**!
        ///
        /// To be fair the code I tried to run was:
        ///
        /// ```rust
        /// File::open("doesn't exist")?
        /// ```
        ///
        /// * Look
        /// * `At`
        /// * This
        ///
        /// ## Tables
        ///
        /// | Maybe | This | Table | Helps |
        /// |-------|:----:|:------|-------|
        /// |hello  | 3.14 | 42    |world  |
        /// |hello  | 3.14 | 42    |world  |
        File::open("doesn't exist")?
    };
    let mut reader = BufReader::new(file);
    let mut buffer = String::new();

    /// Could be a decoding error
    reader.read_to_string(&mut buffer)?;

    println!("{}", buffer);

    Ok(())
}
