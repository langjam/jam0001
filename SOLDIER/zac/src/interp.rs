use anyhow::{anyhow, bail};
use dyn_partial_eq::*;
use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::parser;
use crate::parser::{Assignment, Block, Comment, Expr, FunctionCall, If, Ref, While};
use dyn_clone::DynClone;
use itertools::Itertools;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use std::str::from_utf8;

#[derive(Debug, Clone)]
pub struct Interpreter {
    scope: Rc<RefCell<Scope>>,
    comments: HashMap<String, String>,
}

const BUILTIN_COMMENTS: &[&str; 1] = &["#help"];
pub fn builtin_comment(interpreter: &Interpreter, name: &str) -> Option<String> {
    match name {
        "help" => Some(generate_help_text(interpreter)),
        _ => None,
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let mut scope = Scope::new(None);
        scope.insert("set".into(), Value::Function(Box::new(SetBuiltin {})));
        scope.insert("add".into(), Value::Function(Box::new(AddBuiltin {})));
        scope.insert("mul".into(), Value::Function(Box::new(MulBuiltin {})));
        scope.insert("eq".into(), Value::Function(Box::new(EqBuiltin {})));
        scope.insert("lt".into(), Value::Function(Box::new(LtBuiltin {})));
        scope.insert("gt".into(), Value::Function(Box::new(GtBuiltin {})));
        scope.insert("not".into(), Value::Function(Box::new(NotBuiltin {})));
        scope.insert("and".into(), Value::Function(Box::new(AndBuiltin {})));
        scope.insert("or".into(), Value::Function(Box::new(OrBuiltin {})));
        scope.insert("print".into(), Value::Function(Box::new(PrintBuiltin {})));
        scope.insert("show".into(), Value::Function(Box::new(ShowBuiltin {})));
        scope.insert("chr".into(), Value::Function(Box::new(ChrBuiltin {})));
        scope.insert("cat".into(), Value::Function(Box::new(CatBuiltin {})));
        scope.insert("true".into(), Value::Bool(true));
        scope.insert("false".into(), Value::Bool(false));

        Self {
            scope: Rc::new(RefCell::new(scope)),
            comments: HashMap::new(),
        }
    }

    pub fn new_scope(&self) -> Self {
        let new_scope = Scope::new(Some(Rc::clone(&self.scope)));
        // clone is the wrong choice here, we need to actually share comments between all Scopes
        let mut new_interp = self.clone();
        new_interp.scope = Rc::new(RefCell::new(new_scope));
        new_interp
    }

    pub fn comments(&self) -> impl Iterator<Item = (&str, &str)> {
        self.comments.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }

    pub fn add_comment(&mut self, comment: &Comment) -> anyhow::Result<()> {
        if let Some(name) = &comment.name {
            if self.comments.contains_key(name) {
                bail!("duplicate comment: {}", name);
            }
            self.comments.insert(name.into(), comment.body.clone());
        }
        Ok(())
    }

    pub fn interp(&mut self, expr: &Expr) -> anyhow::Result<Value> {
        let val = match expr {
            Expr::Block(block) => {
                let mut exprs = block.exprs();
                let first = exprs.next().ok_or(anyhow!("a block can't be empty"))?;
                let mut res = self.interp(first)?;
                for expr in exprs {
                    res = self.interp(expr)?;
                }
                res
            }
            Expr::Comment(Comment { name: _, body }) => Value::String(body.into()),
            Expr::Assignment(Assignment { r#ref, expr }) => {
                let val = self.interp(expr)?;
                match r#ref {
                    Ref::CommentRef(comment_name) => {
                        let comment = self.comments.get_mut(comment_name).ok_or_else(|| {
                            anyhow!("couldn't find comment with name {}", comment_name)
                        })?;
                        *comment = val.as_str()?.into();
                    }
                    Ref::VarRef(name) => {
                        self.scope.borrow_mut().insert(name.into(), val.clone());
                    }
                }
                val
            }
            Expr::IntLiteral(n) => Value::Int(*n),
            Expr::Ref(r#ref) => self.get_ref(r#ref)?,
            // XXX:
            // this is lols but we'll use func call syntax to index into strings and maps
            // (don't have lists yet)
            Expr::FunctionCall(FunctionCall { r#ref, args }) => {
                let var = self.get_ref(r#ref)?;
                let args = args
                    .iter()
                    .map(|e| self.interp(e))
                    .collect::<anyhow::Result<Vec<_>>>()?;
                match var {
                    Value::Function(func) => func.call(self, &args)?,
                    Value::String(s) => {
                        let index = get_arg(&args, 0)?.as_num()?;
                        if index < 0 {
                            Value::Bool(false)
                        } else {
                            s.chars()
                                .nth(index as usize)
                                .map(|c| Value::String(c.into()))
                                .unwrap_or(Value::Bool(false))
                        }
                    }
                    Value::Bool(_) | Value::Map(_) | Value::Int(_) => {
                        bail!("tried to call a {:?}", var)
                    }
                }
            }
            Expr::While(While { cond, block }) => {
                // TODO: need to make aa new scope for a new block
                let mut count = 0;
                while self.interp(cond)?.as_bool()? {
                    self.interp(&Expr::Block(block.clone()))?;
                    count += 1;
                }
                Value::Int(count)
            }
            Expr::If(If { cond, block }) => {
                // TODO: need to make aa new scope for a new block
                let b = self.interp(cond)?.as_bool()?;
                if b {
                    self.interp(&Expr::Block(block.clone()))?;
                }
                Value::Bool(b)
            }
            Expr::FuncDef(func_def) => {
                let val = Value::Function(Box::new(FuncDef::from_expr(func_def.clone())));
                self.scope
                    .borrow_mut()
                    .insert(func_def.name.clone(), val.clone());
                val
            }
        };
        Ok(val)
    }

    // TODO: this should probably be a refcell
    fn get_ref(&self, r#ref: &Ref) -> anyhow::Result<Value> {
        match r#ref {
            Ref::CommentRef(name) => {
                let comment_body = self
                    .comments
                    .get(name)
                    .ok_or_else(|| anyhow!("undefined comment {}", name))?;
                Ok(Value::String(comment_body.clone()))
            }
            Ref::VarRef(name) => self
                .scope
                .borrow()
                .get(name)
                .ok_or_else(|| anyhow!("undefined name {}", name))
                .map(|val| val.clone()),
        }
    }
}

#[derive(Debug)]
struct Scope {
    prev: Option<Rc<RefCell<Scope>>>,
    this: BTreeMap<String, Value>,
}

impl Scope {
    fn new(prev: Option<Rc<RefCell<Scope>>>) -> Self {
        Self {
            prev,
            this: Default::default(),
        }
    }

    pub fn insert(&mut self, name: String, val: Value) {
        self.this.insert(name, val);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.this.get(name) {
            return Some(val.clone());
        }

        self.prev
            .as_ref()
            .and_then(|scope| scope.borrow().get(name))
    }
}

#[dyn_partial_eq]
pub trait Function: Debug + DynClone {
    fn call(&self, interp: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value>;
}

dyn_clone::clone_trait_object!(Function);

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Map(BTreeMap<Value, Value>),
    Int(i128),
    Function(Box<dyn Function>),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
struct FuncDef {
    block: Block,
    arg_names: Vec<String>,
}

impl FuncDef {
    fn from_expr(func_def: parser::FuncDef) -> Self {
        Self {
            block: func_def.block,
            arg_names: func_def.arg_names,
        }
    }
}

impl Function for FuncDef {
    fn call(&self, interp: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let mut new_interp = interp.new_scope();
        for (name, val) in self.arg_names.iter().zip(args) {
            new_interp
                .scope
                .borrow_mut()
                .insert(name.to_owned(), val.clone());
        }
        new_interp.interp(&Expr::Block(self.block.clone()))
    }
}

impl Value {
    fn as_func(&self) -> anyhow::Result<&dyn Function> {
        match self {
            Value::Function(f) => Ok(f.as_ref()),
            otherwise => bail!("{:?} is not a function", otherwise),
        }
    }

    fn as_num(&self) -> anyhow::Result<i128> {
        match self {
            Value::Int(i) => Ok(*i),
            otherwise => bail!("{:?} is not an integer", otherwise),
        }
    }

    fn as_bool(&self) -> anyhow::Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            otherwise => bail!("{:?} is not a bool", otherwise),
        }
    }

    fn as_str(&self) -> anyhow::Result<&str> {
        match self {
            Value::String(s) => Ok(s),
            otherwise => bail!("{:?} is not a String", otherwise),
        }
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct SetBuiltin {}
impl Function for SetBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let str = get_arg(args, 0)?.as_str()?;
        let index = get_arg(args, 1)?.as_num()?;
        let new = get_arg(args, 2)?.as_str()?;
        let (left, right) = str.split_at(index as usize);
        Ok(Value::String(format!("{}{}{}", left, new, &right[1..])))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct AddBuiltin {}
impl Function for AddBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let lhs = get_arg(args, 0)?.as_num()?;
        let rhs = get_arg(args, 1)?.as_num()?;
        Ok(Value::Int(lhs + rhs))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct MulBuiltin {}
impl Function for MulBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let lhs = get_arg(args, 0)?.as_num()?;
        let rhs = get_arg(args, 1)?.as_num()?;
        Ok(Value::Int(lhs * rhs))
    }
}

fn get_arg(args: &[Value], n: usize) -> anyhow::Result<&Value> {
    args.get(n).ok_or_else(|| {
        anyhow!(
            "not enough arguments, was looking for {} but only {} were provided",
            n,
            args.len()
        )
    })
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct EqBuiltin {}
impl Function for EqBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let lhs = get_arg(args, 0)?;
        let rhs = get_arg(args, 1)?;
        Ok(Value::Bool(lhs == rhs))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct GtBuiltin {}
impl Function for GtBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let lhs = get_arg(args, 0)?.as_num()?;
        let rhs = get_arg(args, 1)?.as_num()?;
        Ok(Value::Bool(lhs > rhs))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct LtBuiltin {}
impl Function for LtBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let lhs = get_arg(args, 0)?.as_num()?;
        let rhs = get_arg(args, 1)?.as_num()?;
        //println!("{:?} < {:?}", lhs, rhs);
        Ok(Value::Bool(lhs < rhs))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct NotBuiltin {}
impl Function for NotBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let val = get_arg(args, 0)?.as_bool()?;
        Ok(Value::Bool(!val))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct AndBuiltin {}
impl Function for AndBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let lhs = get_arg(args, 0)?.as_bool()?;
        let rhs = get_arg(args, 1)?.as_bool()?;
        Ok(Value::Bool(lhs && rhs))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct OrBuiltin {}
impl Function for OrBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let lhs = get_arg(args, 0)?.as_bool()?;
        let rhs = get_arg(args, 1)?.as_bool()?;
        Ok(Value::Bool(lhs || rhs))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct PrintBuiltin {}
impl Function for PrintBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let val = get_arg(args, 0)?;
        println!("{:?}", val);
        Ok(val.clone())
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct CatBuiltin {}
impl Function for CatBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let mut acc = String::new();
        for arg in args {
            let str = arg.as_str()?;
            acc.push_str(str);
        }
        Ok(Value::String(acc))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct ChrBuiltin {}
impl Function for ChrBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let val = get_arg(args, 0)?.as_num()?.to_le_bytes()[0];
        Ok(Value::String(from_utf8(&[val])?.to_string()))
    }
}

#[derive(Debug, Clone, DynPartialEq, PartialEq)]
struct ShowBuiltin {}
impl Function for ShowBuiltin {
    fn call(&self, _: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let val = get_arg(args, 0)?;
        let s = match val {
            Value::String(s) => s.clone(),
            Value::Map(m) => format!("{:?}", m),
            Value::Int(n) => n.to_string(),
            Value::Function(_) => "<function>".to_string(),
            Value::Bool(true) => "true".to_string(),
            Value::Bool(false) => "false".to_string(),
        };
        Ok(Value::String(s))
    }
}

const WELCOME_TEXT: &str = r#"Help for the Zac Programming Language (https://github.com/sumeet/Zac)

Define a comment with the first line set to an identifier (like #help) and it
will be a string usable inside of your program. You can read from it, and if
you write to it, the change will be reflected inside the source file."#;

fn generate_help_text(interp: &Interpreter) -> String {
    let mut function_names = vec![];
    let mut variable_names = vec![];
    for (name, builtin) in &interp.scope.borrow().this {
        if builtin.as_func().is_ok() {
            function_names.push(name.to_string());
        } else {
            variable_names.push(name.to_string())
        }
    }
    let mut comment_names = BTreeSet::new();
    for comment in BUILTIN_COMMENTS {
        comment_names.insert(comment.to_string());
    }
    for comment in interp.comments.keys() {
        comment_names.insert(format!("#{}", comment));
    }

    let mut txt = String::new();
    txt.push_str(WELCOME_TEXT);
    txt.push_str("\n\nBuiltin functions:\n");
    txt.push_str(&tableize(&function_names));
    txt.push_str("\nAvailable variables\n");
    txt.push_str(&tableize(&variable_names));
    txt.push_str("\nAvailable comments\n");
    txt.push_str(&tableize(&comment_names.into_iter().collect_vec()).to_string());
    txt.trim_end().into()
}

fn tableize(function_names: &[String]) -> String {
    format!("  {}", function_names.iter().join("  "))
}
