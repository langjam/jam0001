use std::slice::Iter;
use std::cell::RefCell;
use std::rc::Rc;
use thiserror::Error;

use crate::ast::{Program, Statement, FileHeader, DefinitionHeader, FunctionDefinition};
use crate::expression::{Expression};
use crate::environment::Environment;
use crate::value::{Value, Function};
use crate::document::Document;

#[derive(Debug)]
pub struct Interpreter<'i> {
    program: Iter<'i, Statement>,
    document: Document,
    environment: Rc<RefCell<Environment>>,
    header: Option<Statement>,
}

impl<'i> Interpreter<'i> {

    pub fn new(program: Iter<'i, Statement>) -> Self {
        Self {
            program,
            document: Document::new("".to_owned()),
            environment: Rc::new(RefCell::new(Environment::new())),
            header: None,
        }
    }

    pub fn setup_document(&mut self) -> Result<(), InterpreterError> {
        match self.program.next() {
            Some(Statement::FileHeader(FileHeader { name, description, author })) => {
                self.document.set_name(name);
                self.document.set_description(description.clone());
                self.document.set_author(author.clone());
            },
            _ => return Err(InterpreterError::MissingFileHeader)
        };

        Ok(())
    }

    pub fn execute_statement(&mut self, statement: Statement) -> Result<(), InterpreterError> {
        match statement {
            Statement::FileHeader(_) => unreachable!(),
            h @ Statement::DefinitionHeader(DefinitionHeader { .. }) => {
                if self.header.is_some() {
                    return Err(InterpreterError::UnusedDefinitionHeader);
                } else {
                    self.header = Some(h);
                }
            },
            Statement::FunctionDefinition(FunctionDefinition { body }) => {
                if self.header.is_none() {
                    return Err(InterpreterError::MissingDefinitionHeader);
                }

                let (name) = match self.header.as_ref().unwrap() {
                    Statement::DefinitionHeader(DefinitionHeader { identifier }) => (identifier),
                    _ => unreachable!(),
                };

                self.environment.borrow_mut().set(name.to_string(), &Value::Function(Function { body }))
            },
            Statement::Expression(expression) => {
                self.execute_expression(expression)?;
            },
            _ => todo!()
        };

        Ok(())
    }

    // TODO: Actually do something here. It's useless at the moment.
    // TODO: Also implemented native functions so that we can test println, etc.
    pub fn execute_expression(&mut self, expression: Expression) -> Result<Option<Value>, InterpreterError> {
        
    }

    pub fn execute(&mut self) -> Result<(), InterpreterError> {
        self.setup_document()?;

        if crate::has_flag("--help") {
            self.document.help();
        }

        while let Some(s) = self.program.next() {
            self.execute_statement(s.clone())?;
        }

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("Could not find a valid file header.")]
    MissingFileHeader,

    #[error("A definition header with no definition was encountered.")]
    UnusedDefinitionHeader,

    #[error("All definitions must be preceded with a valid definition header.")]
    MissingDefinitionHeader,
}

pub fn interpret<'i>(program: Program) -> Result<(), InterpreterError> {
    let mut interpreter = Interpreter::new(program.iter());

    interpreter.execute()?;

    Ok(())
}