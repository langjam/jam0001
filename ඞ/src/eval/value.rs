use std::collections::HashMap;

use super::EvalResult;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    Unit,
    Int(isize),
    Float(f64),
    String(String),
    VarRef { name: String },
    Ptr(*mut Value),
    Comment { content: String },
    List { elems: Vec<Value> },
    Object(Object),
    Class(Class),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub(crate) name: String,
    pub(crate) fields: HashMap<String, Value>,
    pub(crate) methods: HashMap<String, Object>,
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
