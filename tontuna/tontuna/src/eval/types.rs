use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast;
use super::{Env, Value};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub(crate) struct Str {
    pub(crate) chars: Vec<char>,
}

impl Str {
    pub(crate) fn new(s: &str) -> Self {
        Str { chars: s.chars().collect() }
    }

    fn encode(&self) -> String {
        self.chars.iter().copied().collect()
    }

    pub(crate) fn lookup_field(&self, as_value: &Value, field: &str) -> Option<Value> {
        match field {
            "len" => Some(Value::Int(self.chars.len() as i64)),
            "get" => {
                let as_value = as_value.clone();
                Some(Value::NativeFunc(Rc::new(NativeFunc::new1("get", move |idx| {
                    super::intrinsics::string_get(&as_value, idx)
                }))))
            }
            "substring" => {
                let as_value = as_value.clone();
                Some(Value::NativeFunc(Rc::new(NativeFunc::new2("substring", move |idx, len| {
                    super::intrinsics::substring(&as_value, idx, len)
                }))))
            }
            _ => None,
        }
    }
}

impl std::fmt::Debug for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.encode().fmt(f)
    }
}

impl std::fmt::Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.encode().fmt(f)
    }
}

pub(crate) struct NativeFunc {
    pub(crate) name: String,
    pub(crate) f: Box<dyn Fn(&Rc<str>, &[Value]) -> Result<Value, String>>,
}

impl NativeFunc {
    pub(crate) fn new(
        name: impl Into<String>,
        f: impl Fn(&[Value]) -> Result<Value, String> + 'static,
    ) -> NativeFunc {
        NativeFunc {
            name: name.into(),
            f: Box::new(move |_, values| f(values)),
        }
    }

    pub(crate) fn new_src_hack(
        name: impl Into<String>,
        f: impl Fn(Rc<str>) -> Result<Value, String> + 'static,
    ) -> NativeFunc {
        let name = name.into();
        NativeFunc {
            name: name.clone(),
            f: Box::new(move |src, values| {
                match values {
                    [] => f(src.clone()),
                    _ => Err(format!(
                        "{} expects 0 arguments, got {}",
                        name,
                        values.len(),
                    )),
                }
            }),
        }
    }

    pub(crate) fn new1(
        name: impl Into<String>,
        f: impl Fn(&Value) -> Result<Value, String> + 'static,
    ) -> NativeFunc {
        let name = name.into();
        NativeFunc {
            name: name.clone(),
            f: Box::new(move |_, values| {
                match values {
                    [single] => f(single),
                    _ => Err(format!(
                        "{} expects 1 argument, got {}",
                        name,
                        values.len(),
                    )),
                }
            }),
        }
    }

    pub(crate) fn new2(
        name: impl Into<String>,
        f: impl Fn(&Value, &Value) -> Result<Value, String> + 'static,
    ) -> NativeFunc {
        let name = name.into();
        NativeFunc {
            name: name.clone(),
            f: Box::new(move |_, values| {
                match values {
                    [a, b] => f(a, b),
                    _ => Err(format!(
                        "{} expects 2 arguments, got {}",
                        name,
                        values.len(),
                    )),
                }
            }),
        }
    }

    pub(crate) fn new3(
        name: impl Into<String>,
        f: impl Fn(&Value, &Value, &Value) -> Result<Value, String> + 'static,
    ) -> NativeFunc {
        let name = name.into();
        NativeFunc {
            name: name.clone(),
            f: Box::new(move |_, values| {
                match values {
                    [a, b, c] => f(a, b, c),
                    _ => Err(format!(
                        "{} expects 3 arguments, got {}",
                        name,
                        values.len(),
                    )),
                }
            }),
        }
    }
}

pub(crate) struct UserFunc {
    pub(crate) name: String,
    pub(crate) def: Rc<ast::FnDef>,
    pub(crate) env: Env,
}

impl UserFunc {
    pub(crate) fn new(name: String, def: Rc<ast::FnDef>, env: Env) -> UserFunc {
        UserFunc { name, def, env }
    }
}

pub(crate) struct Struct {
    pub(crate) name: String,
    pub(crate) ctor: Option<Rc<NativeFunc>>,
}

pub(crate) struct Instance {
    pub(crate) ty: Rc<Struct>,
    pub(crate) fields: RefCell<HashMap<String, Value>>,
}

impl Instance {
    pub(crate) fn lookup_field(&self, field: &str) -> Option<Value> {
        self.fields.borrow().get(field).cloned()
    }

    pub(crate) fn set_field(&self, field: &str, value: Value) {
        self.fields.borrow_mut().insert(field.to_owned(), value);
    }
}

#[derive(Default)]
pub(crate) struct List {
    pub(crate) values: RefCell<Vec<Value>>,
}

impl List {
    pub fn new(values: Vec<Value>) -> Self {
        List { values: RefCell::new(values) }
    }

    pub(crate) fn lookup_field(&self, as_value: &Value, field: &str) -> Option<Value> {
        match field {
            "len" => Some(Value::Int(self.values.borrow().len() as i64)),
            "get" => {
                let as_value = as_value.clone();
                Some(Value::NativeFunc(Rc::new(NativeFunc::new1("get", move |idx| {
                    super::intrinsics::list_get(&as_value, idx)
                }))))
            }
            "push" => {
                let as_value = as_value.clone();
                Some(Value::NativeFunc(Rc::new(NativeFunc::new1("push", move |val| {
                    super::intrinsics::list_push(&as_value, val)
                }))))
            }
            _ => None,
        }
    }
}

pub(crate) struct Stmt {
    pub(crate) source: Rc<str>,
    pub(crate) ast: Rc<ast::Stmt>,
}

impl Stmt {
    pub(crate) fn is_code(&self) -> bool {
        match *self.ast {
            ast::Stmt::While { .. } |
            ast::Stmt::If { .. } |
            ast::Stmt::Expr { .. } |
            ast::Stmt::For { .. } |
            ast::Stmt::Return { .. } |
            ast::Stmt::Let { .. } |
            ast::Stmt::FnDef(_) |
            ast::Stmt::StructDef { .. } |
            ast::Stmt::Block(_) => true,
            ast::Stmt::Comment(_) => false,
        }
    }

    pub(crate) fn lookup_field(&self, as_value: &Value, field: &str) -> Option<Value> {
        match field {
            "text" => {
                let span = self.ast.span();
                let text = &self.source[span.source_range()];
                Some(Value::Str(Rc::new(Str::new(text))))
            }
            "children" => {
                let as_value = as_value.clone();
                Some(super::intrinsics::stmt_children(self))
            }
            _ => None,
        }
    }
}

pub(crate) struct Interpreter {
    pub(crate) eval: RefCell<super::Evaluator>,
    pub(crate) env: RefCell<Env>,
}

impl Interpreter {
    pub(crate) fn new(src: Rc<str>) -> Value {
        let eval = super::Evaluator::new(src, None, Box::new(std::io::sink()));
        let env = eval.globals.clone();
        Value::Interpreter(Rc::new(Interpreter {
            eval: RefCell::new(eval),
            env: RefCell::new(env),
        }))
    }

    pub(crate) fn lookup_field(&self, as_value: &Value, field: &str) -> Option<Value> {
        match field {
            "run" => {
                let as_value = as_value.clone();
                Some(Value::NativeFunc(Rc::new(NativeFunc::new1("run", move |val| {
                    super::intrinsics::interpreter_run(&as_value, val)
                }))))
            }
            _ => None,
        }
    }
}
