use std::collections::{HashMap, LinkedList};

#[derive(Default)]
pub struct Scope<V> {
    vars: LinkedList<V>,
    index: HashMap<String, *mut V>,
}

impl<V> Scope<V> {
    pub fn new() -> Self {
        Self {
            vars: LinkedList::new(),
            index: HashMap::new(),
        }
    }

    pub fn declare_or_reassign(&mut self, name: impl AsRef<str> + Into<String>, value: V) {
        match self.index.get(name.as_ref()) {
            Some(ptr) => unsafe { **ptr = value },
            None => {
                self.vars.push_back(value);
                let ptr = self.vars.back_mut().unwrap();
                self.index.insert(name.into(), ptr);
            }
        }
    }

    #[inline]
    pub fn get_mut(&mut self, name: &str) -> Option<*mut V> {
        self.index.get(name).copied()
    }

    #[inline]
    pub fn get(&self, name: &str) -> Option<*const V> {
        self.index.get(name).map(|ptr| *ptr as *const _)
    }
}

pub struct ScopeMap<V> {
    scopes: Vec<Scope<V>>,
}

impl<V> ScopeMap<V> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn exit_scope(&mut self) {
        assert!(
            !self.scopes.is_empty(),
            "Trying to leave a scope that does not exist"
        );
        self.scopes.pop();
    }

    pub fn declare_or_reassign(&mut self, name: impl AsRef<str> + Into<String>, value: V) {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_or_reassign(name, value);
    }

    pub fn get_mut(&mut self, name: impl AsRef<str>) -> Option<*mut V> {
        let name = name.as_ref();
        let mut i = self.scopes.len();
        while i > 0 {
            i = i - 1;
            if let Some(value) = self.scopes[i].get_mut(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&V> {
        let name = name.as_ref();
        let mut i = self.scopes.len();
        while i > 0 {
            i = i - 1;
            if let Some(value) = self.scopes[i].get(name) {
                return Some(unsafe { &*value });
            }
        }
        None
    }
}
