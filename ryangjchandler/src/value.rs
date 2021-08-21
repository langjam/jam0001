use std::fmt::{Debug, Result, Formatter, Display};

use crate::ast::Statement;
use crate::interpreter::Interpreter;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Function(Function),
    Bool(bool),
    Const(Box<Value>),
    Null,
}

impl Value {
    pub fn is_type(&self, r#type: impl Into<String>) -> bool {
        let t = r#type.into();

        match (t.as_str(), self) {
            ("string", Self::String(_)) => true,
            ("number", Self::Number(_)) => true,
            ("bool", Self::Bool(_)) => true,
            (_, Self::Const(constant)) => constant.is_type(t),
            _ => false,
        }
    }

    pub fn type_string(&self) -> String {
        match self {
            Self::String(_) => "string".to_owned(),
            Self::Number(_) => "number".to_owned(),
            Self::Bool(_) => "bool".to_owned(),
            Self::Null => "null".to_owned(),
            Self::Const(constant) => constant.type_string(),
            _ => todo!()
        }
    }

    pub fn to_bool(&self) -> bool {
        match self {
            Self::String(s) => ! s.is_empty(),
            Self::Number(n) => *n > 0.0,
            Self::Bool(b) => *b,
            Self::Null => false,
            Self::Function(_) => true,
            Self::Const(constant) => constant.to_bool(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Self::String(s) => s.clone(),
            Self::Function(_) => format!("{:?}", self),
            Self::Number(n) => n.to_string(),
            Self::Null => "null".to_owned(),
            Self::Bool(true) => "true".to_owned(),
            Self::Bool(false) => "false".to_owned(),
            Self::Const(constant) => format!("{}", constant),
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