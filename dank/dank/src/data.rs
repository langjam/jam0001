use std::{borrow::Cow, cell::RefCell, collections::HashMap};

use crate::{ast::Function, eval::Signal};

pub type Ptr<T> = std::rc::Rc<T>;
pub type ObjPtr<'a> = Ptr<RefCell<Object<'a>>>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Object<'s> {
    pub fields: HashMap<Cow<'s, str>, Value<'s>>,
}

impl<'s> Object<'s> {
    #[inline]
    pub(crate) fn move_on_heap(self) -> ObjPtr<'s> {
        ObjPtr::new(RefCell::new(self))
    }
}

pub struct NativeFn<'s> {
    pub name: Cow<'s, str>,
    pub arity: u8, // todo: variadics
    // TODO: This needs a trait
    pub func: Box<dyn Fn(Vec<Value<'s>>) -> Signal<'s>>,
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

impl<'s> From<f64> for Value<'s> {
    fn from(n: f64) -> Self {
        Value::Num(n)
    }
}
impl<'s> From<bool> for Value<'s> {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl<'s> std::fmt::Display for Value<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::Str(s) => write!(f, "{}", s),
            Value::Fn(func) => write!(f, "<fn: {}>", func.name),
            Value::NativeFn(func) => write!(f, "<native fn: {}>", func.name),
            Value::Obj(obj) => {
                write!(f, "{{ ")?;
                let obj = obj.borrow();
                for (i, (k, v)) in obj.fields.iter().enumerate() {
                    write!(
                        f,
                        "{}: {}",
                        k,
                        match v {
                            Value::Str(s) => format!("\"{}\"", s),
                            _ => v.to_string(),
                        }
                    )?;
                    if i != obj.fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}
