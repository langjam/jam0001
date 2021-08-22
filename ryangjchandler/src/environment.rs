use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            parent: None
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<Self>>) -> Self {
        Self {
            store: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn is_constant(&self, name: impl Into<String>) -> bool {
        match self.get(name) {
            Some(Value::Const(_)) => true,
            _ => false,
        }
    }

    pub fn has(&self, name: impl Into<String>) -> bool {
        self.store.contains_key(&name.into())
    }

    pub fn set(&mut self, name: impl Into<String>, value: &Value) {
        self.store.insert(name.into(), value.clone());
    }

    pub fn get(&self, name: impl Into<String>) -> Option<Value> {
        let name = name.into();

        match self.store.get(&name.clone()) {
            Some(v) => Some(v.clone()),
            None => match self.parent {
                Some(ref parent) => parent.borrow().get(name),
                None => None
            }
        }
    }

    pub fn forget(&mut self, name: impl Into<String>) {
        self.store.remove(&name.into());
    }
}