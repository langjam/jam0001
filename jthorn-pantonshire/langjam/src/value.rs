use std::fs::File;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Void,
    Bool(bool),
    Int(i64),
    String(String),
    File(Rc<File>),
}
