use std::{fs::File, io::Read};

use codespan_reporting::files::Error;

use crate::error::report_parse_error;
use crate::frontend::Expr;
use crate::frontend::parser;

pub struct Parser<'a> {
    pub filename: &'a str,
    pub file_contents: String,
}

impl<'a> Parser<'a> {
    pub fn new(filename: &'a str) -> Result<Parser<'a>, String> {
        let mut contents = String::new();
        match File::open(filename) {
            Ok(mut file) => {
                match file.read_to_string(&mut contents) {
                    Ok(_) => {}
                    Err(e) => { return Err(e.to_string()); }
                }
            }
            Err(e) => { return Err(e.to_string()); }
        }
        Ok(Self {
            filename: filename,
            file_contents: contents,
        })
    }

    pub fn parse_file(&mut self) -> Result<Expr, String> {
        let expr = parser::function(self.file_contents.as_str());
        match expr {
            Ok(expr) => { Ok(expr) }
            Err(err) => {
                let res = report_parse_error(self.filename, self.file_contents.as_str(), err);
                if res.is_err() {
                    Err("Could not parse file".to_owned())
                } else {
                    Ok(Expr::NoExpr)
                }
            }
        }
    }
}