use std::{collections::HashMap, fmt};

use super::EvalResult;

#[derive(Clone, PartialEq)]
pub enum Value {
    None,
    Unit,
    Bool(bool),
    Int(isize),
    Float(f64),
    String(String),
    VarRef { name: String },
    Ptr(*mut Value),
    Comment { content: String },
    List { elems: Vec<Value> },
    Object(Object),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub(crate) name: String,
    pub(crate) fields: HashMap<String, Value>,
    pub(crate) methods: HashMap<String, Function>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            fields: HashMap::default(),
            methods: HashMap::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) body: Vec<Value>,
}

impl Function {
    pub fn new(name: String) -> Self {
        Self {
            name,
            params: Vec::new(),
            body: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub(crate) class: String,
    pub(crate) fields: HashMap<String, Value>,
}

impl Object {
    pub fn get_field(&self, name: &str) -> EvalResult {
        self.fields
            .get(name)
            .cloned()
            .ok_or(format!("Unknown field: {}", name))
    }

    pub fn get_field_mut(&mut self, name: &str) -> Result<&mut Value, String> {
        self.fields
            .get_mut(name)
            .ok_or(format!("Unknown field: {}", name))
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::None => write!(f, "None"),
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(ff) => write!(f, "{}", ff),
            Value::String(s) => write!(f, r#""{}""#, s),
            Value::VarRef { name } => write!(f, "{}", name),
            Value::Ptr(ptr) => write!(f, "*{}", unsafe { &**ptr }),
            Value::Comment { content } => write!(f, "/* {} */", content),
            Value::List { elems } => f.debug_list().entries(elems).finish(),
            Value::Object(o) => {
                writeln!(f, "{} {{", &o.class)?;
                for (name, value) in &o.fields {
                    writeln!(f, "{}: {},", name, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::None => write!(f, "None"),
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(ff) => write!(f, "{}", ff),
            Value::String(s) => write!(f, "{}", s),
            Value::VarRef { name } => write!(f, "{}", name),
            Value::Ptr(ptr) => write!(f, "*{}", unsafe { &**ptr }),
            Value::Comment { content } => write!(f, "/* {} */", content),
            Value::List { elems } => f.debug_list().entries(elems).finish(),
            Value::Object(o) => {
                writeln!(f, "{} {{", &o.class)?;
                for (name, value) in &o.fields {
                    writeln!(f, "{}: {},", name, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}
