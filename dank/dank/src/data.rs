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

// pub trait NativeObj<'s> {
//     fn to_string(&self) -> String;
//     fn get_prop(&self, name: Cow<'s, str>) -> Signal<'s>;
//     fn set_prop(&self, name: Cow<'s, str>) -> Signal<'s>;
//
// }

impl<'s> NativeFn<'s> {
    pub fn create<S: Into<Cow<'s, str>>>(
        name: S,
        arity: u8,
        func: impl Fn(Vec<Value<'s>>) -> Signal<'s> + 'static,
    ) -> Ptr<Self> {
        Ptr::new(Self {
            name: name.into(),
            arity,
            func: Box::new(func),
        })
    }
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value<'s> {
    Num(f64),
    Bool(bool),
    Null,
    Str(Cow<'s, str>),
    Obj(ObjPtr<'s>),
    Fn(Ptr<Function<'s>>),
    NativeFn(Ptr<NativeFn<'s>>),
}

macro_rules! nop_partial_ord {
    ($name:ident) => {
        impl<'s> PartialOrd for $name<'s> {
            fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                None
            }
        }
    };
}
nop_partial_ord!(Object);
nop_partial_ord!(Function);
nop_partial_ord!(NativeFn);

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

macro_rules! impl_arithm_op {
    ($trait:ident, $method:ident, $op:tt) => {
        impl<'s> std::ops::$trait<Value<'s>> for Value<'s> {
            type Output = Signal<'s>;

            fn $method(self, rhs: Value<'s>) -> Self::Output {
                match (self, rhs) {
                    (Value::Num(l), Value::Num(r)) => Value::from(l $op r).into(),
                    (l, r) => crate::eval::Evaluator::bin_op_type_error(&l, &r, "+").into(),
                }
            }
        }
    }
}

impl<'s> std::ops::Add<Value<'s>> for Value<'s> {
    type Output = Signal<'s>;

    fn add(self, rhs: Value<'s>) -> Self::Output {
        match (self, rhs) {
            (Value::Num(l), Value::Num(r)) => Value::from(l + r).into(),
            (Value::Str(l), Value::Str(r)) => Value::Str(format!("{}{}", l, r).into()).into(),
            (l, r) => crate::eval::Evaluator::bin_op_type_error(&l, &r, "+").into(),
        }
    }
}
impl<'s> std::ops::Div<Value<'s>> for Value<'s> {
    type Output = Signal<'s>;

    fn div(self, rhs: Value<'s>) -> Self::Output {
        match (self, rhs) {
            (Value::Num(l), Value::Num(r)) if r != 0.0 => Signal::Value(Value::Num(l / r)),
            (Value::Num(_), Value::Num(r)) if r == 0.0 => {
                crate::eval::EvalError::RuntimeError("Attempted to divide by zero".into()).into()
            }
            (l, r) => crate::eval::Evaluator::bin_op_type_error(&l, &r, "+").into(),
        }
    }
}
impl_arithm_op!(Sub, sub, -);
impl_arithm_op!(Mul, mul, *);

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
impl<'s> From<Ptr<NativeFn<'s>>> for Value<'s> {
    fn from(b: Ptr<NativeFn<'s>>) -> Self {
        Value::NativeFn(b)
    }
}
