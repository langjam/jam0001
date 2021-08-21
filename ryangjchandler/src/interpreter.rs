use std::slice::Iter;
use thiserror::Error;

use crate::ast::{Program, Statement};

#[derive(Debug)]
pub struct Interpreter<'i> {
    program: Iter<'i, Statement>,
}

impl<'i> Interpreter<'i> {
    pub fn new(program: Iter<'i, Statement>) -> Self {
        Self { program }
    }

    pub fn execute(&mut self) -> Result<(), InterpreterError> {
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum InterpreterError {

}

pub fn interpret<'i>(program: Program) -> Result<(), InterpreterError> {
    let mut interpreter = Interpreter::new(program.iter());

    interpreter.execute()?;

    Ok(())
}