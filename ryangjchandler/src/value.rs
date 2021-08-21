use std::fmt::{Debug, Result, Formatter, Display};

use crate::ast::Statement;
use crate::interpreter::Interpreter;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Function(Function),
    Null,
}

impl Value {
    pub fn is_type(&self, r#type: impl Into<String>) -> bool {
        match (r#type.into().as_str(), self) {
            ("string", Value::String(_)) => true,
            _ => todo!(),
        }
    }

    pub fn type_string(&self) -> String {
        match self {
            Self::String(_) => "string".to_owned(),
            _ => todo!()
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Self::String(s) => s.clone(),
            Self::Function(_) => format!("{:?}", self),
            Self::Null => "null".to_owned(),
        })
    }
}

#[derive(Clone)]
pub enum Function {
    User {
        name: String,
        params: Vec<(String, Option<String>)>,
        body: Vec<Statement>
    },
    Native {
        name: String,
        callback: NativeFunction,
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Native { name, .. } => write!(f, "NativeFunction<{}>", name),
            Self::User { name, .. } => write!(f, "Function<{}>", name),
        }
    }
}

pub type NativeFunction = fn (&mut Interpreter, Vec<Value>) -> Value;