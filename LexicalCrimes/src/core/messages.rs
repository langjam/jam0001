use std::io::{BufWriter, Write};
use std::io::Result;

use colored::*;

/// single message stdout handler
pub fn put_stdout(message: String) {
    let stdout = std::io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    
    let writeln = writeln!(
        stdout,
        "{}",
        message,
    );

    match writeln {
        Ok(()) => (),
        Err(error) => panic!("Couldn't write to `stdout`: {:?}", error),
    };    
}

/// iteration based flushed stdout handler
pub fn put_message(messages: Vec<String>) -> Result<()> {
    let stdout = std::io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());

    for message in messages {
        let writeln = writeln!(stdout, "{}", message);

        match writeln {
            Ok(()) => (),
            Err(error) => panic!("Couldn't write to `stdout`: {:?}", error),
        };
    }

    stdout.flush()?;

    Ok(())
}

/// push error message
pub fn push_error(message: String) {
    put_stdout(
        format!(
            "{} {}",
            "[!] ERROR:".red().bold(), message.red()
        )
    )
}

/// push warning message
pub fn push_warning(message: String) {
    put_stdout(
        format!(
            "{} {}",
            "[!] WARNING:".yellow().bold(), message.yellow()
        )
    )
}

/// push help banner
pub fn push_help() {
    // banner message is embedded into the binary at compile-time
    const HELP: &str = include_str!("../utils/banner");
    put_stdout(
        format!(
            "{}",
            HELP.green()
        )
    )
}