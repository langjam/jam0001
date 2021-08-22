mod intrinsics;
mod types;

use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};
use crate::{ast::{self, TokenKind}, Span};
use self::types::{Instance, Interpreter, List, NativeFunc, Stmt, Str, Struct, UserFunc};

#[derive(Clone)]
pub(crate) enum Value {
    Nil,
    Int(i64),
    Bool(bool),
    Str(Rc<Str>),
    NativeFunc(Rc<NativeFunc>),
    Struct(Rc<Struct>),
    Instance(Rc<Instance>),
    List(Rc<List>),
    UserFunc(Rc<UserFunc>),
    Stmt(Rc<Stmt>),
    Interpreter(Rc<Interpreter>),
}

impl From<NativeFunc> for Value {
    fn from(v: NativeFunc) -> Self {
        Self::NativeFunc(Rc::new(v))
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<Str> for Value {
    fn from(v: Str) -> Value {
        Self::Str(v.into())
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Value {
        Self::Str(Rc::new(Str::new(v)))
    }
}

impl Value {
    fn type_name(&self) -> String {
        match self {
            Value::Nil => "nil".to_owned(),
            Value::Int(_) => "Int".to_owned(),
            Value::Bool(_) => "Bool".to_owned(),
            Value::Str(_) => "Str".to_owned(),
            Value::NativeFunc(_) => "NativeFn".to_owned(),
            Value::Struct(_) => "Struct".to_owned(),
            Value::Instance(i) => i.ty.name.clone(),
            Value::List(_) => "List".to_owned(),
            Value::UserFunc(_) => "Fn".to_owned(),
            Value::Stmt(s) if s.is_code() => "Code".to_owned(),
            Value::Stmt(_) => "Comment".to_owned(),
            Value::Interpreter(_) => "Interpreter".to_owned(),
        }
    }

    fn lookup_field(&self, field: &str) -> Option<Value> {
        match self {
            Value::Str(s) => s.lookup_field(self, field),
            Value::Instance(i) => i.lookup_field(field),
            Value::List(l) => l.lookup_field(self, field),
            Value::Stmt(s) => s.lookup_field(self, field),
            Value::Interpreter(s) => s.lookup_field(self, field),
            _ => None,
        }
    }

    fn set_field(&self, field: &str, value: Value) -> Result<(), String> {
        match self {
            Value::Str(_) => Err("Str fields cannot be modified".to_owned()),
            Value::Instance(i) => Ok(i.set_field(field, value)),
            Value::List(_) => Err("List fields cannot be modified".to_owned()),
            Value::Stmt(s) if s.is_code() => Err("Code fields cannot be modified".to_owned()),
            Value::Stmt(s) => Err("Comment fields cannot be modified".to_owned()),
            Value::Interpreter(_) => Err("Interpreter fields cannot be modified".to_owned()),
            _ => Err(format!("{} cannot have fields", self.type_name())),
        }
    }

    fn stringify(&self) -> String {
        match self {
            Value::Nil => "nil".to_owned(),
            Value::Int(x) => x.to_string(),
            Value::Bool(x) => x.to_string(),
            Value::Str(x) => x.to_string(),
            Value::NativeFunc(f) => format!("<Native {}>", f.name),
            Value::Struct(s) => format!("<Struct {}>", s.name),
            Value::Instance(i) => format!("<{}>", i.ty.name),
            Value::List(_) => "<List>".to_owned(),
            Value::UserFunc(f) => format!("<Fn {}>", f.name),
            Value::Stmt(s) if s.is_code() => "<Code>".to_owned(),
            Value::Stmt(_) => "<Comment>".to_owned(),
            Value::Interpreter(_) => "<Interpreter>".to_owned(),
        }
    }
}

pub(crate) struct RuntimeError {
    pub(crate) message: String,
    pub(crate) span: Option<Span>,
}

enum EvalStop {
    Error(RuntimeError),
    Return(Value),
}

impl From<RuntimeError> for EvalStop {
    fn from(v: RuntimeError) -> Self {
        Self::Error(v)
    }
}

pub(crate) struct EnvEntry {
    name: String,
    value: RefCell<Value>,
    next: Env,
}

#[derive(Clone)]
pub(crate) enum Env {
    Chain(Rc<EnvEntry>),
    GlobalFence(Rc<Env>),
    Global(Rc<RefCell<HashMap<String, Value>>>),
}

impl Env {
    fn global(globals: HashMap<String, Value>) -> Env {
        Env::Global(Rc::new(globals.into()))
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        let mut this = self;
        loop {
            match this {
                Env::Chain(entry) => {
                    if entry.name == name {
                        return Some(entry.value.borrow().clone());
                    }
                    this = &entry.next;
                }
                Env::GlobalFence(env) => this = env,
                Env::Global(globals) => return globals.borrow().get(name).cloned(),
            }
        }
    }

    fn with_fence(&self) -> Env {
        match self {
            Env::Chain(_) |
            Env::GlobalFence(_) => self.clone(),
            Env::Global(_) => Env::GlobalFence(Rc::new(self.clone())),
        }
    }

    fn define(&self, name: &str, value: Value) -> Env {
        match self {
            Env::Chain { .. } |
            Env::GlobalFence(_) => {
                Env::Chain(Rc::new(EnvEntry {
                    name: name.to_owned(),
                    value: value.into(),
                    next: self.clone(),
                }))
            }
            Env::Global(globals) => {
                globals.borrow_mut().insert(name.to_owned(), value);
                self.clone()
            }
        }
    }

    fn set(&self, name: &str, value: Value) -> Result<(), ()> {
        let mut this = self;
        loop {
            match this {
                Env::Chain(entry) => {
                    if entry.name == name {
                        (*entry.value.borrow_mut()) = value;
                        return Ok(())
                    }
                    this = &entry.next;
                }
                Env::GlobalFence(env) => this = env,
                Env::Global(globals) => {
                    return match globals.borrow_mut().get_mut(name) {
                        Some(v) => {
                            *v = value;
                            Ok(())
                        }
                        None => {
                            Err(())
                        }
                    }
                }
            }
        }
    }
}

struct BuiltinTypes {
    nil: Rc<Struct>,
    int: Rc<Struct>,
    bool: Rc<Struct>,
    str: Rc<Struct>,
    list: Rc<Struct>,
    strukt: Rc<Struct>,
    func: Rc<Struct>,
    code: Rc<Struct>,
    comment: Rc<Struct>,
    interpreter: Rc<Struct>,
    all: Vec<Rc<Struct>>,
}

impl BuiltinTypes {
    fn new() -> Self {
        fn make_ty(name: &str) -> Rc<Struct> {
            Rc::new(Struct {
                name: name.to_owned(),
                ctor: Some(Rc::new(NativeFunc::new(name, |_| {
                    Err(intrinsics::invalid_ctor())
                }))),
            })
        }
        let mut builtins = BuiltinTypes {
            nil: make_ty("Nil"),
            int: make_ty("Int"),
            bool: make_ty("Bool"),
            str: Rc::new(Struct {
                name: "Str".to_owned(),
                ctor: Some(Rc::new(NativeFunc::new("Str", |values| {
                    Ok(intrinsics::str_ctor(values))
                }))),
            }),
            list: Rc::new(Struct {
                name: "List".to_owned(),
                ctor: Some(Rc::new(NativeFunc::new("List", |values| {
                    Ok(intrinsics::list_ctor(values))
                }))),
            }),
            strukt: make_ty("Struct"),
            func: make_ty("Fn"),
            code: make_ty("Code"),
            comment: make_ty("Comment"),
            interpreter: Rc::new(Struct {
                name: "Interpreter".to_owned(),
                ctor: Some(Rc::new(NativeFunc::new_src_hack("Interpreter", |src| {
                    Ok(Interpreter::new(src))
                }))),
            }),
            all: Vec::new(),
        };
        builtins.all = vec![
            builtins.nil.clone(),
            builtins.int.clone(),
            builtins.bool.clone(),
            builtins.str.clone(),
            builtins.list.clone(),
            builtins.strukt.clone(),
            builtins.func.clone(),
            builtins.code.clone(),
            builtins.comment.clone(),
            builtins.interpreter.clone(),
        ];
        builtins
    }
}

pub(crate) struct Evaluator {
    source: Rc<str>,
    globals: Env,
    call_stack_size: u64,
    builtins: BuiltinTypes,
}

impl Evaluator {
    pub(crate) fn new(source: Rc<str>, program: Option<&ast::Program>, output: Box<dyn Write>) -> Evaluator {
        let mut globals = HashMap::new();
        let output = Rc::new(RefCell::new(output));
        let output2 = output.clone();
        globals.insert("print".to_owned(), NativeFunc::new("print", move |values| {
            let mut output = output2.borrow_mut();
            intrinsics::print(values, &mut **output)?;
            Ok(Value::Nil)
        }).into());
        let output2 = output.clone();
        globals.insert("println".to_owned(), NativeFunc::new("println", move |values| {
            let mut output = output2.borrow_mut();
            intrinsics::println(values, &mut **output)?;
            Ok(Value::Nil)
        }).into());
        globals.insert("panic".to_owned(), NativeFunc::new("panic", move |values| {
            Err(intrinsics::panic(values))
        }).into());
        if let Some(program) = program {
            let stmts = Value::List(Rc::new(List::new(
                program.code.stmts
                    .iter()
                    .map(|s| Value::Stmt(Rc::new(Stmt {
                        source: source.clone(),
                        ast: s.clone(),
                    })))
                    .collect(),
            )));
            globals.insert("program_source".to_owned(), NativeFunc::new("program_souce", move |values| {
                if values.len() != 0 {
                    return Err(format!("program_source expects 0 args, got {}", values.len()));
                }
                Ok(stmts.clone())
            }).into());
        }
        let builtins = BuiltinTypes::new();
        for value in &builtins.all {
            globals.insert(value.name.clone(), Value::Struct(value.clone()));
        }
        Evaluator {
            source,
            globals: Env::global(globals),
            call_stack_size: 0,
            builtins,
        }
    }

    fn eval_statement(&mut self, stmt: &ast::Stmt, env: &Env) -> Result<Env, EvalStop> {
        match stmt {
            ast::Stmt::While { cond, body, .. } => {
                loop {
                    let (is_true, binding) = self.eval_if_cond(cond, env)?;
                    if is_true {
                        match binding {
                            Some((name, value)) => {
                                let env = env.with_fence().define(self.token_source(name), value);
                                self.eval_block(&body.contents, &env)?;
                            }
                            None => {
                                self.eval_block(&body.contents, env)?;
                            }
                        }
                    } else {
                        break;
                    }
                }
            }
            ast::Stmt::If { cond, body, tail, .. } => {
                let (is_true, binding) = self.eval_if_cond(cond, env)?;
                if is_true {
                    match binding {
                        Some((name, value)) => {
                            let env = env.with_fence().define(self.token_source(name), value);
                            self.eval_block(&body.contents, &env)?;
                        }
                        None => {
                            self.eval_block(&body.contents, env)?;
                        }
                    }
                } else {
                    self.eval_if_tail(tail, env)?;
                }
            }
            ast::Stmt::Expr { expr, .. } => {
                self.eval_expr(expr, env)?;
            }
            ast::Stmt::For { name, iterable, body, .. } => {
                let iter = self.eval_expr(iterable, env)?;
                let list = match iter {
                    Value::List(list) => list,
                    _ => return Err(EvalStop::Error(RuntimeError {
                        message: format!("cannot iterate over {}", iter.type_name()),
                        span: Some(iterable.span()),
                    })),
                };
                let values = list.values.borrow().clone();
                for item in &values {
                    let iter_env = env.define(self.token_source(*name), item.clone());
                    self.eval_block(&body.contents, &iter_env)?;
                }
            }
            ast::Stmt::Return { ret, value, .. } => {
                if self.call_stack_size == 0 {
                    return Err(EvalStop::Error(RuntimeError {
                        message: "cannot use return outside of a function".to_owned(),
                        span: Some(ret.span),
                    }));
                }
                let value = self.eval_expr(value, env)?;
                return Err(EvalStop::Return(value));
            }
            ast::Stmt::Let { name, value, .. } => {
                let value = self.eval_expr(value, env)?;
                return Ok(env.define(self.token_source(*name), value));
            }
            ast::Stmt::Comment(_) => {}
            ast::Stmt::FnDef(def) => {
                let name = self.token_source(def.name);
                let func = UserFunc {
                    name: name.to_owned(),
                    def: def.clone(),
                    env: env.clone(),
                };
                return Ok(env.define(name, Value::UserFunc(Rc::new(func))));
            }
            ast::Stmt::StructDef { name, fns, .. } => {
                if fns.len() > 0 {
                    todo!("struct with fns");
                }
                let name = self.token_source(*name);
                let strukt = Struct {
                    name: name.to_owned(),
                    ctor: None,
                };
                return Ok(env.define(name, Value::Struct(Rc::new(strukt))));
            }
            ast::Stmt::Block(block) => {
                self.eval_block(&block.contents, env)?;
            }
        }
        Ok(env.clone())
    }

    fn eval_if_cond(&mut self, cond: &ast::IfCond, env: &Env) -> Result<(bool, Option<(ast::Token, Value)>), RuntimeError> {
        match cond {
            ast::IfCond::Expr(e) => Ok((self.eval_cond(e, env)?, None)),
            ast::IfCond::TypeTest { name, ty, value, .. } => {
                let ty_val = self.eval_expr(ty, env)?;
                let expected = match ty_val {
                    Value::Struct(s) => s.clone(),
                    _ => {
                        return Err(RuntimeError {
                            message: format!("type evaluated to {}", ty_val.type_name()),
                            span: Some(ty.span()),
                        });
                    }
                };
                let value = self.eval_expr(value, env)?;
                let value_ty = self.value_type(&value);
                Ok((Rc::ptr_eq(&value_ty, &expected), Some((*name, value))))
            }
        }
    }

    fn eval_if_tail(&mut self, tail: &ast::IfTail, env: &Env) -> Result<(), EvalStop> {
        match tail {
            ast::IfTail::None => Ok(()),
            ast::IfTail::Else { body, .. } => self.eval_block(&body.contents, env),
            ast::IfTail::ElseIf { cond, body, tail, .. } => {
                let (is_true, binding) = self.eval_if_cond(cond, env)?;
                if is_true {
                    match binding {
                        Some((name, value)) => {
                            let env = env.with_fence().define(self.token_source(name), value);
                            self.eval_block(&body.contents, &env)?;
                        }
                        None => {
                            self.eval_block(&body.contents, env)?;
                        }
                    }
                } else {
                    self.eval_if_tail(tail, env)?;
                }
                Ok(())
            }
        }
    }

    fn eval_expr(&mut self, expr: &ast::Expr, env: &Env) -> Result<Value, RuntimeError> {
        match expr {
            ast::Expr::Name { name } => {
                let text = self.token_source(*name);
                match env.lookup(text) {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError {
                        message: format!("undefined variable: {}", text),
                        span: Some(name.span),
                    }),
                }
            }
            ast::Expr::Number { value, .. } => Ok((*value).into()),
            ast::Expr::Bool { value, .. } => Ok((*value).into()),
            ast::Expr::Str { value, .. } => Ok(value.as_str().into()),
            ast::Expr::Nil { .. } => Ok(Value::Nil),
            ast::Expr::SelfExpr { tok } => todo!(),
            ast::Expr::Call { func, args, .. } => {
                let func = self.eval_expr(func, env)?;
                let mut eval_args = || -> Result<Vec<Value>, RuntimeError> {
                    args.iter()
                        .map(|arg| self.eval_expr(&arg.item, env))
                        .collect()
                };
                match func {
                    Value::NativeFunc(f) => {
                        let args = eval_args()?;
                        (f.f)(&self.source, &args).map_err(|message| RuntimeError {
                            message,
                            span: Some(expr.span()),
                        })
                    }
                    Value::Struct(s) => {
                        if let Some(ctor) = &s.ctor {
                            let args = eval_args()?;
                            (ctor.f)(&self.source, &args).map_err(|message| RuntimeError {
                                message,
                                span: Some(expr.span()),
                            })
                        } else {
                            if args.len() > 0 {
                                return Err(RuntimeError {
                                    message: format!(
                                        "{} expects 0 args, got {}",
                                        s.name,
                                        args.len(),
                                    ),
                                    span: Some(expr.span()),
                                });
                            }
                            Ok(Value::Instance(Rc::new(Instance {
                                ty: s.clone(),
                                fields: Default::default(),
                            })))
                        }
                    }
                    Value::UserFunc(f) => {
                        let args = eval_args()?;
                        let mut call_env = f.env.with_fence();
                        if args.len() != f.def.params.len() {
                            return Err(RuntimeError {
                                message: format!(
                                    "{} expects {} args, got {}",
                                    f.name,
                                    f.def.params.len(),
                                    args.len()
                                ),
                                span: Some(expr.span()),
                            });
                        }
                        for (arg, param) in args.into_iter().zip(&f.def.params) {
                            call_env = call_env.define(self.token_source(param.item), arg);
                        }
                        self.call_stack_size += 1;
                        let result = match self.eval_block(&f.def.body.contents, &call_env) {
                            Ok(()) => Ok(Value::Nil),
                            Err(EvalStop::Error(e)) => Err(e),
                            Err(EvalStop::Return(val)) => Ok(val),
                        };
                        self.call_stack_size -= 1;
                        result
                    }
                    other => {
                        Err(RuntimeError {
                            message: format!("{} cannot be called", other.type_name()),
                            span: Some(expr.span()),
                        })
                    }
                }
            }
            ast::Expr::Paren { inner, .. } => self.eval_expr(inner, env),
            ast::Expr::BinOp { lhs, operator, rhs } => {
                match operator.kind {
                    TokenKind::And => {
                        return if self.eval_cond(lhs, env)? {
                            Ok(self.eval_cond(rhs, env)?.into())
                        } else {
                            Ok(false.into())
                        };
                    }
                    TokenKind::Or => {
                        return if self.eval_cond(lhs, env)? {
                            Ok(true.into())
                        } else {
                            Ok(self.eval_cond(rhs, env)?.into())
                        };
                    }
                    _ => {}
                }
                let lhs = self.eval_expr(lhs, env)?;
                let rhs = self.eval_expr(rhs, env)?;
                let result = match operator.kind {
                    TokenKind::Plus => intrinsics::add(&lhs, &rhs),
                    TokenKind::Minus => intrinsics::sub(&lhs, &rhs),
                    TokenKind::Star => intrinsics::mul(&lhs, &rhs),
                    TokenKind::Slash => intrinsics::div(&lhs, &rhs),
                    TokenKind::Less => Ok(intrinsics::less(&lhs, &rhs)),
                    TokenKind::LessEq => Ok(intrinsics::less_eq(&lhs, &rhs)),
                    TokenKind::Greater => Ok(intrinsics::greater(&lhs, &rhs)),
                    TokenKind::GreaterEq => Ok(intrinsics::greater_eq(&lhs, &rhs)),
                    TokenKind::EqEq => Ok(intrinsics::eq(&lhs, &rhs)),
                    TokenKind::NotEq => Ok(intrinsics::not_eq(&lhs, &rhs)),
                    x => panic!("invalid operator: {:?}", x),
                };
                result.map_err(|message| RuntimeError {
                    message,
                    span: Some(expr.span()),
                })
            }
            ast::Expr::Field { obj, field, .. } => {
                let obj = self.eval_expr(obj, env)?;
                let field_name = self.token_source(*field);
                match obj.lookup_field(field_name) {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError {
                        message: format!(
                            "{} does not have field `{}`",
                            obj.type_name(),
                            field_name,
                        ),
                        span: Some(field.span),
                    }),
                }
            }
            ast::Expr::AssignVar { name, value, .. } => {
                let value = self.eval_expr(value, env)?;
                match env.set(self.token_source(*name), value.clone()) {
                    Ok(()) => Ok(value),
                    Err(()) => Err(RuntimeError {
                        message: "undefined variable".to_owned(),
                        span: Some(name.span),
                    }),
                }
            }
            ast::Expr::AssignField { obj, field, value, .. } => {
                let obj = self.eval_expr(obj, env)?;
                let value = self.eval_expr(value, env)?;
                match obj.set_field(&self.token_source(*field), value.clone()) {
                    Ok(()) => Ok(value),
                    Err(message) => Err(RuntimeError {
                        message,
                        span: Some(field.span),
                    }),
                }
            }
        }
    }

    fn eval_cond(&mut self, cond: &ast::Expr, env: &Env) -> Result<bool, RuntimeError> {
        match self.eval_expr(cond, env)? {
            Value::Bool(b) => Ok(b),
            other => Err(RuntimeError {
                message: format!("condition evaluated to a {}", other.type_name()),
                span: Some(cond.span()),
            }),
        }
    }

    fn eval_block(&mut self, block: &ast::NakedBlock, env: &Env) -> Result<(), EvalStop> {
        let mut env = env.with_fence();
        for stmt in &block.stmts {
            env = self.eval_statement(stmt, &env)?;
        }
        Ok(())
    }

    fn token_source(&self, token: ast::Token) -> &str {
        &self.source[token.span.source_range()]
    }

    fn value_type(&self, value: &Value) -> Rc<Struct> {
        match value {
            Value::Nil => self.builtins.nil.clone(),
            Value::Int(_) => self.builtins.int.clone(),
            Value::Bool(_) => self.builtins.bool.clone(),
            Value::Str(_) => self.builtins.str.clone(),
            Value::NativeFunc(_) => self.builtins.func.clone(),
            Value::Struct(_) => self.builtins.strukt.clone(),
            Value::Instance(i) => i.ty.clone(),
            Value::List(_) => self.builtins.list.clone(),
            Value::UserFunc(_) => self.builtins.func.clone(),
            Value::Stmt(s) => if s.is_code() {
                self.builtins.code.clone()
            } else {
                self.builtins.comment.clone()
            },
            Value::Interpreter(_) => self.builtins.interpreter.clone(),
        }
    }

    pub(crate) fn run_program(&mut self, program: &ast::Program) -> Result<(), RuntimeError> {
        let mut env = self.globals.clone();
        for stmt in &program.code.stmts {
            match self.eval_statement(stmt, &env) {
                Ok(e) => env = e,
                Err(EvalStop::Error(e)) => return Err(e),
                Err(EvalStop::Return(_)) => panic!("return outside of function"),
            }
        }
        Ok(())
    }
}
