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
    ObjectRef(Box<Value>),
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

#[derive(Clone, PartialEq)]
pub struct Object {
    pub class: String,
    pub fields: HashMap<String, Value>,
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

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut this = f.debug_struct(&self.class);
        for (name, value) in &self.fields {
            this.field(name, value);
        }
        this.finish()
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut this = f.debug_struct(&self.class);
        for (name, value) in &self.fields {
            this.field(name, value);
        }
        this.finish()
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
            Value::Object(o) => write!(f, "{:?}", o),
            Value::ObjectRef(o) => write!(f, "ref {:?}", o),
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
            Value::Object(o) => write!(f, "{}", o),
            Value::ObjectRef(o) => write!(f, "ref {}", o),
        }
    }
}
