use std::{borrow::Cow, cell::RefCell, collections::HashMap};

pub type Ptr<T> = std::rc::Rc<T>;
pub type ObjPtr<'a> = Ptr<RefCell<Object<'a>>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Object<'s> {
    pub fields: HashMap<String, Value<'s>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'s> {
    Num(f64),
    Bool(bool),
    Null,
    Str(Cow<'s, str>),
    Obj(ObjPtr<'s>),
}
