use std::slice::Iter;
use std::cell::RefCell;
use std::rc::Rc;
use thiserror::Error;

use crate::ast::{Program, Statement, FileHeader, DefinitionHeader, FunctionDefinition, If, While, Var, Const};
use crate::expression::{Expression, Call, Assign};
use crate::environment::Environment;
use crate::value::{Value, Function, NativeFunction};
use crate::document::Document;
use crate::token::TokenKind;

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
            Some(Statement::FileHeader(FileHeader { name, description, author, version })) => {
                self.document.set_name(name);
                self.document.set_description(description.clone());
                self.document.set_author(author.clone());
                self.document.set_version(version.clone());
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
            Statement::Var(Var { init }) => {
                if self.header.is_none() {
                    return Err(InterpreterError::MissingDefinitionHeader);
                }

                let (name, r#type) = match self.header.as_ref().unwrap() {
                    Statement::DefinitionHeader(DefinitionHeader { identifier, r#type, .. }) => (identifier.clone(), r#type.clone()),
                    _ => unreachable!()
                };

                let value = match self.execute_expression(init)? {
                    Some(v) => v,
                    None => unreachable!()
                };

                if let Some(t) = r#type {
                    if ! value.is_type(t.clone()) {
                        return Err(InterpreterError::VariableOrConstantTypeError(name.clone(), t, value.type_string()));
                    }
                }

                self.environment.borrow_mut().set(name, &value);

                self.header = None;
            },
            Statement::Const(Const { init }) => {
                if self.header.is_none() {
                    return Err(InterpreterError::MissingDefinitionHeader);
                }
                
                let (name, r#type) = match self.header.as_ref().unwrap() {
                    Statement::DefinitionHeader(DefinitionHeader { identifier, r#type, .. }) => (identifier.clone(), r#type.clone()),
                    _ => unreachable!()
                };

                let value = match self.execute_expression(init)? {
                    Some(v) => v,
                    None => unreachable!()
                };

                if let Some(t) = r#type {
                    if ! value.is_type(t.clone()) {
                        return Err(InterpreterError::VariableOrConstantTypeError(name.clone(), t, value.type_string()));
                    }
                }

                self.environment.borrow_mut().set(name, &Value::Const(Box::new(value)));

                self.header = None;
            },
            Statement::FunctionDefinition(FunctionDefinition { body }) => {
                if self.header.is_none() {
                    return Err(InterpreterError::MissingDefinitionHeader);
                }

                let (name, params) = match self.header.as_ref().unwrap() {
                    Statement::DefinitionHeader(DefinitionHeader { identifier, params, .. }) => (identifier, params.clone()),
                    _ => unreachable!(),
                };

                self.environment.borrow_mut().set(name, &Value::Function(Function::User {
                    name: name.into(),
                    params,
                    body
                }));

                self.header = None;
            },
            Statement::Expression(expression) => {
                self.execute_expression(expression)?;
            },
            Statement::If(If { condition, then, otherwise }) => {
                match self.execute_expression(condition)? {
                    Some(value) => {
                        if value.to_bool() {
                            for statement in then {
                                self.execute_statement(statement)?;
                            }
                        } else {
                            for statement in otherwise {
                                self.execute_statement(statement)?;
                            }
                        }
                    },
                    None => unreachable!()
                }
            },
            Statement::While(While { condition, then }) => {
                loop {
                    match self.execute_expression(condition.clone())? {
                        Some(value) => {
                            if value.to_bool() {
                                for statement in &then {
                                    self.execute_statement(statement.clone())?;
                                }
                            } else {
                                break
                            }
                        },
                        None => unreachable!()
                    }
                }
            }
            _ => todo!()
        };

        Ok(())
    }

    pub fn execute_expression(&mut self, expression: Expression) -> Result<Option<Value>, InterpreterError> {
        Ok(Some(match expression {
            Expression::Call(Call { function, args }) => {
                let args: Vec<Value> = args.iter()
                    .map(|a| {
                        let value = self.execute_expression(a.clone());

                        if value.is_err() {
                            crate::error(&format!("{}", value.err().unwrap()));
                            std::process::exit(1);
                        } else {
                            value.unwrap()
                        }
                    })
                    .map(|a| a.unwrap())
                    .collect();

                let function = self.execute_expression(*function)?;

                match function {
                    Some(Value::Function(Function::User { name: function_name, body, params, .. })) => {
                        let mut new_environment = Environment::new_with_parent(self.environment.clone());
                        let old_environment = Rc::clone(&self.environment);

                        for (i, (name, param_type)) in params.iter().enumerate() {
                            let arg = &args[i];

                            if let Some(t) = param_type {
                                if ! arg.is_type(t) {
                                    return Err(InterpreterError::ParameterTypeError(name.clone(), function_name, t.to_string(), arg.type_string()));
                                }
                            }

                            new_environment.set(name, &args[i]);
                        }

                        self.environment = Rc::new(RefCell::new(new_environment));

                        for statement in body {
                            self.execute_statement(statement)?;
                        }

                        self.environment = old_environment;
                        
                        Value::Null
                    },
                    Some(Value::Function(Function::Native { callback, .. })) => {
                        callback(self, args);

                        Value::Null
                    },
                    None => unreachable!(),
                    _ => {
                        return Err(InterpreterError::ValueIsNotCallable(function.unwrap()));
                    }
                }
            },
            Expression::GetIdentifier(identifier) => {
                let value = self.environment.borrow().get(identifier.clone());

                if value.is_none() {
                    return Err(InterpreterError::UndefinedIdentifier(identifier));
                } else {
                    value.unwrap()
                }
            },
            Expression::String(string) => Value::String(string),
            Expression::Number(number) => Value::Number(number),
            Expression::Bool(b) => Value::Bool(b),
            Expression::Null => Value::Null,
            Expression::Infix(left, operator, right) => {
                let left = self.execute_expression(*left)?.unwrap();
                let right = self.execute_expression(*right)?.unwrap();

                match operator {
                    TokenKind::Plus => {
                        match (left, right) {
                            (Value::String(mut l), Value::String(r)) => {
                                l.push_str(r.as_str());

                                Value::String(l)
                            },
                            (Value::Number(l), Value::Number(r)) => {
                                Value::Number(l + r)
                            }
                            _ => todo!()
                        }
                    },
                    TokenKind::Asterisk => {
                        match (left, right) {
                            (Value::String(l), Value::String(r)) => {
                                return Err(InterpreterError::UnsupportedOperandTypes("string".to_owned(), "*".to_owned(), "string".to_owned()));
                            },
                            (Value::Number(l), Value::Number(r)) => {
                                Value::Number(l * r)
                            }
                            _ => todo!()
                        }
                    },
                    TokenKind::Minus => {
                        match (left, right) {
                            (Value::String(l), Value::String(r)) => {
                                return Err(InterpreterError::UnsupportedOperandTypes("string".to_owned(), "-".to_owned(), "string".to_owned()));
                            },
                            (Value::Number(l), Value::Number(r)) => {
                                Value::Number(l - r)
                            }
                            _ => todo!()
                        }
                    },
                    TokenKind::ForwardSlash => {
                        match (left, right) {
                            (Value::String(l), Value::String(r)) => {
                                return Err(InterpreterError::UnsupportedOperandTypes("string".to_owned(), "/".to_owned(), "string".to_owned()));
                            },
                            (Value::Number(l), Value::Number(r)) => {
                                Value::Number(l / r)
                            }
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            Expression::Assign(Assign { target, value }) => {
                let target = match *target {
                    Expression::GetIdentifier(i) => i,
                    _ => unreachable!(),
                };

                if ! self.environment.borrow().has(target.clone()) {
                    return Err(InterpreterError::AssigningToUndefinedValue(target));
                }

                let value = match self.execute_expression(*value)? {
                    Some(e) => e,
                    None => unreachable!()
                };

                self.environment.borrow_mut().set(target, &value);

                value
            },
            _ => todo!()
        }))
    }

    pub fn register_stdlib_function(&mut self, name: impl Into<String>, callback: NativeFunction) {
        let name = name.into();

        self.environment.borrow_mut().set(name.clone(), &Value::Function(Function::Native {
            name: name.into(),
            callback: callback,
        }))
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

    #[error("All definitions (fn, var, const) must be preceded with a valid definition header.")]
    MissingDefinitionHeader,

    #[error("Trying to access undefined identifier `{0}`.")]
    UndefinedIdentifier(String),

    #[error("Value of type `{0:?}` is not callable.")]
    ValueIsNotCallable(Value),

    #[error("Parameter `{0}` for function `{1}` must be of type `{2}`, received `{3}`.)")]
    ParameterTypeError(String, String, String, String),

    #[error("Variable or constant `{0}` must be of type `{1}`, received `{2}`.")]
    VariableOrConstantTypeError(String, String, String),

    #[error("Unsupported operand types: {0} {1} {2}")]
    UnsupportedOperandTypes(String, String, String),

    #[error("Trying to assign value to undefined variable {0}.")]
    AssigningToUndefinedValue(String),
}

pub fn interpret<'i>(program: Program) -> Result<(), InterpreterError> {
    let mut interpreter = Interpreter::new(program.iter());

    crate::stdlib::register(&mut interpreter);

    interpreter.execute()?;

    Ok(())
}