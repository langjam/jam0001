use std::fmt;
use std::fs::File;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Void,
    Bool(bool),
    Int(i64),
    String(String),
    Function(String),
    // File(Rc<File>),
}

impl Value {
    fn coerce_string(&self) -> Option<String> {
        match self {
            Value::Void => Some("".into()),
            Value::Bool(val) => Some(format!("{}", val)),
            Value::Int(val) => Some(format!("{}", val)),
            Value::String(val) => Some(val.clone()),
            Value::Function(_) => None,
            // Value::File(_) => None,
        }
    }

    fn coerce_int(&self) -> Option<i64> {
        match self {
            Value::Void => Some(0),
            Value::Bool(val) => Some(if *val { 1 } else { 0 }),
            Value::Int(val) => Some(*val),
            Value::String(val) => val.parse().ok(),
            Value::Function(_) => None,
            // Value::File(_) => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, ""),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Int(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            // Value::File(val) => write!(f, "{:?}", val),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Int(lhs), Self::Int(rhs)) => lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            // (Self::File(lhs), Self::File(rhs)) => Rc::ptr_eq(lhs, rhs),

            (Self::String(val), other) | (other, Self::String(val)) => match other.coerce_string() {
                Some(ref coerced) => val == coerced,
                None => false,
            },

            (Self::Int(val), other) | (other, Self::Int(val)) => match other.coerce_int() {
                Some(coerced) => *val == coerced,
                None => false,
            },

            _ => false,
        }
    }
}

impl Eq for Value {}

impl From<bool> for Value {
    fn from(val: bool) -> Self {
        Self::Bool(val)
    }
}

impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Self::Int(val)
    }
}

impl From<String> for Value {
    fn from(val: String) -> Self {
        Self::String(val)
    }
}
