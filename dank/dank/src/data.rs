use std::{borrow::Cow, cell::RefCell, collections::HashMap};

use crate::{ast::Function, eval::EvalError};

pub type Ptr<T> = std::rc::Rc<T>;
pub type ObjPtr<'a> = Ptr<RefCell<Object<'a>>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Object<'s> {
    pub fields: HashMap<String, Value<'s>>,
}

pub struct NativeFn<'s> {
    pub name: Cow<'s, str>,
    pub arity: i8,
    // TODO: This needs a trait
    pub func: Box<dyn Fn(Vec<Value<'s>>) -> Result<Value<'s>, EvalError>>,
}

impl<'s> std::fmt::Debug for NativeFn<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFn")
            .field("name", &self.name)
            .field("func", &"<...>")
            .finish()
    }
}

impl<'s> std::cmp::PartialEq for NativeFn<'s> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'s> {
    Num(f64),
    Bool(bool),
    Null,
    Str(Cow<'s, str>),
    Obj(ObjPtr<'s>),
    Fn(Ptr<Function<'s>>),
    NativeFn(Ptr<NativeFn<'s>>),
}
