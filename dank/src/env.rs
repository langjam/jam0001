//! Taken directly from https://github.com/ves-lang/ves/blob/master/ves-middle/src/env.rs.
use std::collections::HashMap;

/// The default (average) size of a scope.
const DEFAULT_SCOPE_SIZE: usize = 16;
/// The default (average) max scope depth.
const AVG_MAX_SCOPE_DEPTH: usize = 5;

/// A scope within an [`Env`].
pub type Scope<K, T> = HashMap<K, T>;

/// A scoped environment that uses a [`Vec`] of hashmaps internally.
#[derive(Debug, Clone)]
pub struct Env<K, T> {
    /// The stack of scopes.
    scopes: Vec<Scope<K, T>>,
}

impl<K, T> Env<K, T>
where
    K: std::hash::Hash + Eq,
{
    /// Create a new environment with a single scope on the stack.
    pub fn new() -> Self {
        Self {
            scopes: {
                let mut v = Vec::with_capacity(AVG_MAX_SCOPE_DEPTH);
                v.push(Scope::with_capacity(DEFAULT_SCOPE_SIZE));
                v
            },
        }
    }

    /// Returns `true` if the environment is currently holding the global scope.
    /// This function returns `false` if there are no scopes on the stack.
    #[inline]
    pub fn is_global(&self) -> bool {
        self.scopes.len() == 1
    }

    /// Pushes a new scope onto the stack.
    #[inline]
    pub fn push(&mut self) {
        self.scopes.push(Scope::with_capacity(DEFAULT_SCOPE_SIZE));
    }

    /// Pops off and returns the current scope.
    #[inline]
    pub fn pop(&mut self) -> Option<Scope<K, T>> {
        self.scopes.pop()
    }

    #[inline]
    /// Returns the currently used scope.
    pub fn get_scope(&self) -> Option<&Scope<K, T>> {
        self.scopes.last()
    }

    /// Returns `true` if the given name is "owned" by (was declared in) the current scope.
    #[inline]
    pub fn is_owned_by_current_scope(&self, name: &K) -> bool {
        self.in_current_scope(name).is_some()
    }

    /// Returns a reference to the the entry with the given name in the current scope.
    pub fn in_current_scope(&self, name: &K) -> Option<&T> {
        self.scopes.last().and_then(|s| s.get(name))
    }

    /// Returns a reference to the entry with the given name by traversing the scope chain.
    #[inline]
    pub fn get(&self, name: &K) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name) {
                return Some(&scope[name]);
            }
        }
        None
    }

    /// Returns a mutable reference to the entry with the given name by traversing the scope chain.
    pub fn get_mut(&mut self, name: &K) -> Option<&mut T> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                return scope.get_mut(name);
            }
        }
        None
    }

    /// Adds the given (key, value) pair into the current scopes.
    /// Returns the index of the scope.
    pub fn add(&mut self, name: K, value: T) -> usize {
        self.scopes
            .last_mut()
            .expect("At least one scope is required for insertion")
            .insert(name, value);
        self.scopes.len() - 1
    }

    /// Updates the entry with the given name, returning the previous value.
    pub fn update(&mut self, name: &K, value: T) -> Option<T> {
        for i in (0..self.scopes.len()).rev() {
            let scope = &mut self.scopes[i];
            if scope.contains_key(name) {
                return Some(std::mem::replace(scope.get_mut(name).unwrap(), value));
            }
        }
        None
    }

    /// Removes the entry with the given name, returning the index of its scope.
    pub fn remove(&mut self, name: &K) -> Option<usize> {
        for i in (0..self.scopes.len()).rev() {
            let scope = &mut self.scopes[i];
            if scope.remove(name).is_some() {
                return Some(self.scopes.len() - i - 1);
            }
        }
        None
    }

    /// Updates the deepest entry with the given name if present.
    /// If not, adds it to the current scope.
    pub fn update_or_add(&mut self, name: K, value: T)
    where
        T: Clone,
    {
        if self.update(&name, value.clone()).is_none() {
            self.add(name, value);
        }
    }
}

impl<K, T> Default for Env<K, T>
where
    K: std::hash::Hash + Eq,
{
    fn default() -> Self {
        Self::new()
    }
}
