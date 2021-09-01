use std::collections::HashMap;

pub use self::value::{Class, Function, Object, Value};

mod ast;
mod scope;
use scope::ScopeMap;
mod value;
pub use ast::remove_refs;

pub type EvalResult = Result<Value, String>;

pub struct Evaluator {
    classes: HashMap<String, Class>,
    vars: ScopeMap<Value>,
    fns: ScopeMap<Function>,
    pub(crate) returning: Option<Value>,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut this = Self {
            classes: HashMap::default(),
            vars: ScopeMap::new(),
            fns: ScopeMap::new(),
            returning: None,
        };
        this.register_ast_classes();
        this
    }

    pub(crate) fn enter_scope(&mut self) {
        self.vars.enter_scope();
        self.fns.enter_scope();
    }

    pub(crate) fn exit_scope(&mut self) {
        self.vars.exit_scope();
        self.fns.exit_scope();
    }

    pub fn get_class(&self, name: &str) -> Result<&Class, String> {
        self.classes
            .get(name)
            .ok_or(format!("Unknown class: {}", name))
    }

    pub(crate) fn add_class(&mut self, name: impl Into<String>, class: Class) {
        self.classes.insert(name.into(), class);
    }

    pub fn get_var(&mut self, name: &str) -> Result<*mut Value, String> {
        self.vars
            .get_mut(name)
            .ok_or(format!("Unknown var: {}", name))
    }

    pub(crate) fn create_var(&mut self, name: impl AsRef<str> + Into<String>, value: Value) {
        self.vars.declare_or_reassign(name.into(), value);
    }

    pub fn get_fn(&self, name: &str) -> Result<Function, String> {
        self.fns
            .get(name)
            .cloned()
            .ok_or(format!("Unknown function: {}", name))
    }

    pub(crate) fn create_fn(&mut self, name: impl Into<String>, function: Function) {
        self.fns.declare_or_reassign(name.into(), function);
    }
}
