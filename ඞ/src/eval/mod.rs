use std::collections::HashMap;

use quickscope::ScopeMap;

use self::value::{Class, Object, Value};

mod ast;
mod value;

pub type EvalResult = Result<Value, String>;

pub struct Evaluator {
    classes: HashMap<String, Class>,
    vars: ScopeMap<String, Value>,
    pub(crate) target: Option<Object>,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut this = Self {
            classes: HashMap::default(),
            vars: ScopeMap::new(),
            target: None,
        };
        this.register_ast_classes();
        this
    }

    pub fn get_class(&self, name: &str) -> EvalResult {
        self.classes
            .get(name)
            .map(|class| Value::Class(class.clone()))
            .ok_or(format!("Unknown class: {}", name))
    }

    pub(crate) fn add_class(&mut self, name: impl Into<String>, class: Class) {
        self.classes.insert(name.into(), class);
    }

    pub fn get_var(&mut self, name: &str) -> Result<*mut Value, String> {
        self.vars
            .get_mut(name)
            .map(|val| val as *mut _)
            .ok_or(format!("Unknown var: {}", name))
    }

    pub(crate) fn create_var(&mut self, name: impl Into<String>, value: Value) {
        self.vars.define(name.into(), value);
    }
}
