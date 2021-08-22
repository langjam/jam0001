use crate::{
    ast::{Expression, Statement},
    builtins,
    lexer::{Lexer, Token, TokenType},
    parser::Parser,
};
use gc::{Finalize, Gc, GcCell, Trace};
use joinery::JoinableIterator;
use qp_trie::{wrapper::BString, Trie};
use std::{
    cell::RefCell,
    convert::{Infallible, TryFrom, TryInto},
    ops::{ControlFlow, FromResidual, Try},
};
use std::{fmt, path::PathBuf};

#[derive(Debug, PartialEq)]
pub struct Map {
    trie: Trie<BString, Value>,
}

impl Finalize for Map {}
unsafe impl Trace for Map {
    gc::custom_trace!(this, {
        for (_, v) in this.trie.iter() {
            mark(v);
        }
    });
}

#[derive(Debug, Clone, Trace, Finalize, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Array(Gc<GcCell<Vec<Value>>>),
    Struct(String, Gc<GcCell<Map>>),
    Null,
}

impl Default for Value {
    fn default() -> Value {
        Value::Null
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => f.write_str(s),
            Value::Array(a) => write!(f, "[{}]", a.borrow().iter().join_with(", ")),
            Value::Struct(name, values) => write!(
                f,
                "{} {{{}}}",
                name,
                values
                    .borrow()
                    .trie
                    .iter()
                    .map(|(a, b)| format!("{} := {}", a.as_str(), b))
                    .join_with(", ")
            ),
            Value::Null => f.write_str("<NULL>"),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(n) => *n > 0,
            Value::Float(n) => *n != 0.0,
            Value::String(s) => s.len() > 0,
            Value::Array(a) => a.borrow().len() > 0,
            Value::Struct(..) => true,
            Value::Null => false,
        }
    }

    pub fn describe_type(&self) -> String {
        match self {
            Value::Integer(_) => "an integer".to_owned(),
            Value::Float(_) => "a floating point number".to_owned(),
            Value::String(_) => "a string".to_owned(),
            Value::Array(_) => "an array".to_owned(),
            Value::Struct(name, _) => format!("a struct of type {}", name),
            Value::Null => "NULL".to_owned(),
        }
    }

    // Operator impls:

    pub fn equal(&self, rhs: Value) -> Result<Value, String> {
        Ok(Value::Integer((*self == rhs).into()))
    }

    pub fn not_equal(&self, rhs: Value) -> Result<Value, String> {
        Ok(Value::Integer((*self != rhs).into()))
    }

    pub fn less_than(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a < b).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Integer((a < b).into())),
            _ => Err("Operands of < operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn less_than_eq(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a <= b).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Integer((a <= b).into())),
            _ => Err("Operands of <= operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn greater_than(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a > b).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Integer((a > b).into())),
            _ => Err("Operands of > operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn greater_than_eq(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a >= b).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Integer((a >= b).into())),
            _ => Err("Operands of >= operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn add(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a + b).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float((a + b).into())),
            _ => Err("Operands of + operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn subtract(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a - b).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float((a - b).into())),
            _ => Err("Operands of - operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn multiply(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a * b).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float((a * b).into())),
            _ => Err("Operands of * operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn divide(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => match b {
                0 => Err("Cannot divide an integer by 0".to_owned()),
                _ => Ok(Value::Integer((a / b).into())),
            },
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float((a / b).into())),
            _ => Err("Operands of / operator have to be both integers or both floats".to_owned()),
        }
    }

    pub fn modulo(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => match b {
                0 => Err("Cannot divide an integer by 0".to_owned()),
                _ => Ok(Value::Integer((a % b).into())),
            },
            _ => Err("Operands of % operator have to be both integers".to_owned()),
        }
    }

    pub fn and(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a & b).into())),
            _ => Err("Operands of & operator have to be both integers".to_owned()),
        }
    }

    pub fn or(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer((a | b).into())),
            _ => Err("Operands of | operator have to be both integers".to_owned()),
        }
    }

    pub fn not(&self) -> Result<Value, String> {
        match self {
            Value::Integer(a) => Ok(Value::Integer(!a)),
            _ => Err("Operand of ! operator must be an integer".to_owned()),
        }
    }

    pub fn negate(&self) -> Result<Value, String> {
        match self {
            Value::Integer(a) => Ok(Value::Integer(-a)),
            Value::Float(a) => Ok(Value::Float(-a)),
            _ => Err("Operand of unary '-' operator must be an integer or a float".to_owned()),
        }
    }

    pub fn pow(&self, rhs: Value) -> Result<Value, String> {
        match (self, &rhs) {
            (Value::Integer(a), Value::Integer(b)) => {
                let b: u32 = (*b).try_into().or(Err(
                    "in pow(): integer can only be raised to nonnegative power within u32::MAX"
                        .to_string(),
                ))?;

                Ok(Value::Integer(a.pow(b)))
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.powf(*b))),
            _ => Err("Arguments to pow() have to be both integers of both floats".to_owned()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scope<'s> {
    parent_scope: Option<&'s Scope<'s>>,
    variables: Trie<BString, RefCell<Value>>,
    functions: Trie<BString, Statement>,
    struct_defs: Trie<BString, Trie<BString, ()>>,
}

impl Scope<'_> {
    /// Checks is a variable is defined in the current scope or any parent scopes
    pub fn is_var_defined(&self, ident: &str) -> bool {
        self.variables.iter_prefix_str(ident).next().is_some()
    }

    /// Gets a function, and the scope it was defined in
    pub fn get_fn_and_scope(&self, ident: &String) -> Result<(&Statement, &Scope), String> {
        let mut scope = self;
        loop {
            let mut matches = scope.functions.iter_prefix_str(ident);
            let v = match matches.next() {
                Some(v) => v,
                None => {
                    if let Some(parent) = scope.parent_scope {
                        scope = parent;
                        continue;
                    } else {
                        return Err(format!("no such function: `{}`", ident));
                    }
                }
            };

            for collision in matches {
                if collision.0.as_str().starts_with(v.0.as_str()) {
                    continue;
                }

                return Err(format!(
                    "use of the identifier `{}` is ambiguous, it could refer to `{}` and `{}`",
                    ident,
                    v.0.as_str(),
                    collision.0.as_str()
                ));
            }
            return Ok((v.1, scope));
        }
    }

    pub fn var(&self, ident: &str) -> Result<&RefCell<Value>, String> {
        let mut scope = self;
        loop {
            let mut matches = scope.variables.iter_prefix_str(ident);
            let v = match matches.next() {
                Some(v) => v,
                None => {
                    if let Some(parent) = scope.parent_scope {
                        scope = parent;
                        continue;
                    } else {
                        return Err(format!("no such variable: `{}`", ident));
                    }
                }
            };

            for collision in matches {
                if collision.0.as_str().starts_with(v.0.as_str()) {
                    continue;
                }

                return Err(format!(
                    "use of the identifier `{}` is ambiguous, it could refer to `{}` and `{}`",
                    ident,
                    v.0.as_str(),
                    collision.0.as_str()
                ));
            }
            return Ok(v.1);
        }
    }

    pub fn get_struct_def(&self, ident: &str) -> Result<&Trie<BString, ()>, String> {
        let mut scope = self;
        loop {
            let mut matches = scope.struct_defs.iter_prefix_str(ident);
            let v = match matches.next() {
                Some(v) => v,
                None => {
                    if let Some(parent) = scope.parent_scope {
                        scope = parent;
                        continue;
                    } else {
                        return Err(format!("Struct type `{}` has not been defined", ident));
                    }
                }
            };

            for collision in matches {
                if collision.0.as_str().starts_with(v.0.as_str()) {
                    continue;
                }

                return Err(format!(
                    "use of the struct name `{}` is ambiguous, it could refer to `{}` and `{}`",
                    ident,
                    v.0.as_str(),
                    collision.0.as_str()
                ));
            }
            return Ok(v.1);
        }
    }

    /// Returns vec of exports
    pub fn add_functions_from_ast(&mut self, ast: &[Statement]) -> Vec<Statement> {
        let mut exports = Vec::new();
        for stmt in ast {
            if let Statement::FunctionDefinition {
                name,
                parameters: _,
                body: _,
                export,
            } = &stmt
            {
                self.functions.insert_str(name, stmt.clone());
                if *export {
                    exports.push(stmt.clone());
                }
            };
        }
        return exports;
    }

    /// Returns vec of exports
    pub fn add_struct_defs_from_ast(&mut self, ast: &[Statement]) -> Vec<Statement> {
        let mut exports = Vec::new();
        for stmt in ast {
            if let Statement::StructDefinition {
                struct_type,
                fields,
                export,
            } = &stmt
            {
                self.struct_defs.insert_str(
                    struct_type,
                    fields
                        .iter()
                        .map(|field| (BString::from(field.clone()), ()))
                        .collect(),
                );

                if *export {
                    exports.push(stmt.clone());
                }
            }
        }
        exports
    }

    /// Create a new scope, with `self` as the parent.
    pub fn child_scope(&self) -> Scope<'_> {
        Scope {
            parent_scope: Some(self),
            variables: Trie::new(),
            functions: builtins::builtins(),
            struct_defs: Trie::new(),
        }
    }
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Self {
            parent_scope: Default::default(),
            variables: Default::default(),
            functions: builtins::builtins(),
            struct_defs: Trie::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    message: String,
    line: usize,
    column: usize,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} in line {}, col {}",
            self.message, self.line, self.column
        )
    }
}

#[derive(Debug)]
pub struct Interpreter<'b, 's> {
    scope: Scope<'s>,
    body: &'b [Statement],
    line: usize,
    column: usize,
    variable_exports: Trie<BString, RefCell<Value>>,
    function_exports: Vec<Statement>,
    struct_exports: Vec<Statement>,
}

/// Represents all events that need to be bubbled-up during interpretation:
/// - errors
/// - returns
/// - breaks
/// - continues
#[must_use]
pub enum Exec {
    Ok,
    Return(Value),
    Break,
    Continue,
    Err(InterpreterError),
}

impl Try for Exec {
    type Output = ();
    type Residual = Self;

    fn from_output(_: ()) -> Self {
        Exec::Ok
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Exec::Ok => ControlFlow::Continue(()),
            v => ControlFlow::Break(v),
        }
    }
}

impl FromResidual for Exec {
    fn from_residual(residual: Exec) -> Self {
        residual
    }
}

impl FromResidual<Result<Infallible, InterpreterError>> for Exec {
    fn from_residual(residual: Result<Infallible, InterpreterError>) -> Self {
        match residual {
            Ok(a) => match a {},
            Err(e) => Exec::Err(e),
        }
    }
}

impl<'b, 's> Interpreter<'b, 's> {
    pub fn new(mut scope: Scope<'s>, ast: &'b [Statement]) -> Self {
        let function_exports = scope.add_functions_from_ast(ast);
        let struct_exports = scope.add_struct_defs_from_ast(ast);

        Self {
            scope,
            body: ast,
            line: 0,
            column: 0,
            function_exports,
            struct_exports,
            variable_exports: Trie::new(),
        }
    }

    pub fn with_ast(ast: &'b [Statement]) -> Self {
        Self::new(Default::default(), ast)
    }

    fn get_token_type<'t>(&mut self, token: &'t Token) -> &'t TokenType {
        self.line = token.line;
        self.column = token.column;
        &token.token_type
    }

    fn error(&self, message: String) -> InterpreterError {
        InterpreterError {
            line: self.line,
            column: self.column,
            message,
        }
    }

    fn eval_literal_token(&mut self, token: &Token) -> Result<Value, InterpreterError> {
        match self.get_token_type(token) {
            TokenType::Integer(num_str) => {
                let num = num_str.parse::<i64>().unwrap();
                Ok(Value::Integer(num))
            }
            TokenType::Float(num_str) => {
                let num = num_str.parse::<f64>().unwrap();
                Ok(Value::Float(num))
            }
            TokenType::String(s) => Ok(Value::String(s.to_string())),
            _ => unreachable!(),
        }
    }

    fn eval_variable_token(&mut self, token: &Token) -> Result<&RefCell<Value>, InterpreterError> {
        match self.get_token_type(token) {
            TokenType::Identifier(ident, args) => {
                if args.len() != 0 {
                    return Err(self.error(
                        "variable identifiers cannot contain backtick arguments".to_owned(),
                    ));
                }

                self.scope.var(&ident).map_err(|e| self.error(e))
            }
            _ => unreachable!(),
        }
    }

    fn eval_assignment(
        &mut self,
        target: &Expression,
        expr: &Expression,
    ) -> Result<Value, InterpreterError> {
        let value = self.eval(expr, false)?;
        match target {
            Expression::Variable { name } => {
                *self.eval_variable_token(name)?.borrow_mut() = value.clone()
            }
            Expression::ArrayRef { array, index } => {
                self.with_arrayref(array, index, |arr, i| {
                    arr.borrow_mut().get_mut(i).map(|slot| {
                        *slot = value.clone();
                    })
                })?;
            }
            Expression::StructFieldRef { target, field_name } => {
                self.with_struct_field_ref(target, field_name, |field_value| {
                    *field_value = value.clone();
                })?;
            }
            _ => todo!(),
        }
        Ok(value)
    }

    fn eval_binaryop(
        &mut self,
        operator: &Token,
        left: &Box<Expression>,
        right: &Box<Expression>,
    ) -> Result<Value, InterpreterError> {
        let left_val = self.eval(left, false)?;
        let right_val = self.eval(right, false)?;

        match self.get_token_type(operator) {
            TokenType::Plus => left_val.add(right_val),
            TokenType::Minus => left_val.subtract(right_val),
            TokenType::Star => left_val.multiply(right_val),
            TokenType::Slash => left_val.divide(right_val),
            TokenType::Modulo => left_val.modulo(right_val),
            TokenType::And => left_val.and(right_val),
            TokenType::Or => left_val.or(right_val),
            TokenType::Equal => left_val.equal(right_val),
            TokenType::NotEqual => left_val.not_equal(right_val),
            TokenType::LessThan => left_val.less_than(right_val),
            TokenType::LessThanOrEqual => left_val.less_than_eq(right_val),
            TokenType::GreaterThan => left_val.greater_than(right_val),
            TokenType::GreaterThanOrEqual => left_val.greater_than_eq(right_val),
            _ => unreachable!(),
        }
        .map_err(|e| self.error(e))
    }

    fn eval_unaryop(
        &mut self,
        operator: &Token,
        operand: &Box<Expression>,
    ) -> Result<Value, InterpreterError> {
        let operand_val = self.eval(operand, false)?;

        match self.get_token_type(operator) {
            TokenType::Not => operand_val.not(),
            TokenType::Minus => operand_val.negate(),
            _ => unreachable!(),
        }
        .map_err(|e| self.error(e))
    }

    fn eval_functioncall(
        &mut self,
        callee: &Box<Expression>,
        arguments: &Vec<Expression>,
    ) -> Result<Value, InterpreterError> {
        let function_ident = match callee.as_ref() {
            Expression::Variable { name } => match self.get_token_type(&name) {
                TokenType::Identifier(ident, args) => {
                    if args.len() != 0 {
                        return Err(self.error(
                            "identifiers used to call a function cannot contain backtick arguments"
                                .to_owned(),
                        ));
                    }
                    ident
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let mut evaluated_args = Vec::new();
        for arg in arguments {
            evaluated_args.push(self.eval(arg, false)?);
        }

        let (func_def, func_def_scope) = self
            .scope
            .get_fn_and_scope(function_ident)
            .map_err(|e| self.error(e))?;

        let mut call_scope = func_def_scope.child_scope();

        let (parameters, body): (&Vec<String>, &Box<Statement>) = match func_def {
            Statement::FunctionDefinition {
                name: _,
                parameters,
                body,
                export: _,
            } => (parameters, body),

            Statement::CallBuiltin { function } => {
                return function(evaluated_args).map_err(|e| self.error(e));
            }
            _ => unreachable!(),
        };

        if arguments.len() != parameters.len() {
            return Err(self.error(format!(
                "Incorrect number of arguments for function call, expected {} got {}",
                parameters.len(),
                arguments.len()
            )));
        }

        for (param, arg) in parameters.iter().zip(evaluated_args) {
            call_scope.variables.insert_str(param, RefCell::new(arg));
        }

        let function_ast = match body.as_ref() {
            Statement::Block { statements } => statements,
            _ => unreachable!(),
        };

        let mut function_interpreter = Interpreter::new(call_scope, function_ast);
        function_interpreter.line = self.line;
        function_interpreter.column = self.column;

        let result = function_interpreter.run();
        self.line = function_interpreter.line;
        self.column = function_interpreter.column;

        if !function_interpreter.variable_exports.is_empty()
            || !function_interpreter.function_exports.is_empty()
        {
            return Err(
                self.error("values/functions cannot be exported from within functions".to_string())
            );
        }

        match result {
            Exec::Ok => Ok(Value::Null),
            Exec::Return(v) => Ok(v),
            Exec::Break => Err(self.error("break outside of a loop".to_owned())),
            Exec::Continue => Err(self.error("continue outside of a loop".to_owned())),
            Exec::Err(e) => Err(e),
        }
    }

    fn with_arrayref<R>(
        &mut self,
        array: &Expression,
        index: &Expression,
        f: impl FnOnce(&GcCell<Vec<Value>>, usize) -> Option<R>,
    ) -> Result<R, InterpreterError> {
        let array_value = self.eval(array, false)?;
        let array = match &array_value {
            Value::Array(a) => a,
            v => return Err(self.error(format!("cannot index into {}", v.describe_type()))),
        };

        let index = match self.eval(index, false)? {
            Value::Integer(n) => n,
            v => return Err(self.error(format!("{} cannot be an index", v.describe_type()))),
        };

        let length = array.borrow().len();
        let adjusted = if index < 0 {
            index + length as i64
        } else {
            index
        };
        usize::try_from(adjusted)
            .ok()
            .and_then(|i| f(&array, i))
            .ok_or_else(|| {
                self.error(format!(
                    "index {} is out of bounds for a {}-element array",
                    index, length
                ))
            })
    }

    fn with_struct_field_ref<R>(
        &mut self,
        r#struct: &Expression,
        field_name: &String,
        f: impl FnOnce(&mut Value) -> R,
    ) -> Result<R, InterpreterError> {
        let target = self.eval(r#struct, false)?;

        if let Value::Struct(name, fields) = &target {
            if let Some((_, value)) = fields
                .borrow_mut()
                .trie
                .iter_prefix_mut_str(field_name)
                .next()
            {
                Ok(f(value))
            } else {
                Err(self.error(format!(
                    "Struct {} does not contain field {}",
                    name, field_name,
                )))
            }
        } else {
            Err(self.error(format!("Cannot get a field of {}", target.describe_type())))
        }
    }

    fn eval(&mut self, expr: &Expression, value_ignored: bool) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Literal { value } => self.eval_literal_token(value),
            Expression::Variable { name } => {
                if value_ignored {
                    Ok(Default::default())
                } else {
                    Ok(self.eval_variable_token(name)?.borrow().clone())
                }
            }
            Expression::ArrayLiteral { elements } => {
                if value_ignored {
                    for element in elements {
                        self.eval(element, value_ignored)?;
                    }
                    Ok(Default::default())
                } else {
                    let values = elements
                        .iter()
                        .map(|element| self.eval(element, value_ignored))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Value::Array(Gc::new(GcCell::new(values))))
                }
            }
            Expression::ArrayRef { array, index } => {
                self.with_arrayref(array, index, |arr, i| arr.borrow().get(i).cloned())
            }
            Expression::StructLiteral {
                struct_type,
                fields,
            } => {
                let struct_def = self
                    .scope
                    .get_struct_def(struct_type)
                    .map_err(|msg| self.error(msg))?;

                for (field_name, _) in fields.iter() {
                    match struct_def.iter_prefix_str(field_name).count() {
                        0 => {
                            return Err(self.error(format!(
                                "Field `{}` does not exist in struct type `{}`",
                                field_name, struct_type
                            )))
                        }
                        1 => (),
                        _ => {
                            return Err(self.error(format!(
                                "Field name `{}` is ambiguous in struct type `{}`",
                                field_name, struct_type
                            )))
                        }
                    }
                }

                let missing_fields: Vec<_> = struct_def
                    .keys()
                    .filter(|struct_field| {
                        !fields
                            .iter()
                            .any(|(field_name, _)| struct_field.as_str().starts_with(field_name))
                    })
                    .map(|field| field.as_str())
                    .collect();

                if missing_fields.len() > 0 {
                    return Err(self.error(format!(
                        "Struct literal `{}` missing fields: {}",
                        struct_type,
                        missing_fields.iter().join_with(", ")
                    )));
                }

                if value_ignored {
                    for (_, value) in fields.iter() {
                        self.eval(value, value_ignored)?;
                    }
                    Ok(Default::default())
                } else {
                    let fields = fields
                        .into_iter()
                        .map(|(name, value)| {
                            Ok((
                                BString::from(name.clone()),
                                self.eval(value, value_ignored)?,
                            ))
                        })
                        .collect::<Result<Trie<_, _>, _>>()?;

                    Ok(Value::Struct(
                        struct_type.clone(),
                        Gc::new(GcCell::new(Map { trie: fields })),
                    ))
                }
            }
            Expression::StructFieldRef { target, field_name } => {
                self.with_struct_field_ref(target, field_name, |field_value| field_value.clone())
            }
            Expression::Assignment { target, value } => self.eval_assignment(target, value),
            Expression::BinaryOp {
                operator,
                left,
                right,
            } => self.eval_binaryop(operator, left, right),
            Expression::UnaryOp { operator, right } => self.eval_unaryop(operator, right),
            Expression::FunctionCall { callee, arguments } => {
                self.eval_functioncall(callee, arguments)
            }
        }
    }

    fn import_module(&mut self, name: &String) -> Exec {
        let mut path = PathBuf::new();
        path.set_file_name(name);
        path.set_extension("🌌");

        match std::fs::read_to_string(path) {
            Ok(code) => {
                let lexer = Lexer::new(&code);
                match lexer.into_tokens() {
                    Ok(tokens) => {
                        let parser = Parser::new(tokens);
                        match parser.parse() {
                            Ok(ast) => {
                                let mut interpreter = Interpreter::with_ast(&ast);
                                interpreter.run()?;

                                for func in interpreter.function_exports {
                                    match &func {
                                        Statement::FunctionDefinition {
                                            name,
                                            parameters: _,
                                            body: _,
                                            export: _,
                                        } => {
                                            self.scope.functions.insert_str(name, func.clone());
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                for stru in interpreter.struct_exports {
                                    match &stru {
                                        Statement::StructDefinition {
                                            struct_type,
                                            fields,
                                            export: _,
                                        } => {
                                            self.scope.struct_defs.insert_str(
                                                struct_type,
                                                fields
                                                    .iter()
                                                    .map(|field| (BString::from(field.clone()), ()))
                                                    .collect(),
                                            );
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                self.scope.variables.extend(interpreter.variable_exports);

                                Exec::Ok
                            }
                            Err(e) => {
                                Exec::Err(self.error(format!("Error parsing imported file: {}", e)))
                            }
                        }
                    }
                    Err(e) => Exec::Err(self.error(format!("Error lexing imported file: {}", e))),
                }
            }
            Err(e) => Exec::Err(self.error(format!("Error reading imported file: {}", e))),
        }
    }

    pub fn run(&mut self) -> Exec {
        for stmt in self.body {
            match stmt {
                Statement::Expression { expr } => {
                    self.eval(&expr, true)?;
                }

                Statement::Definition { variable, value } => {
                    if let TokenType::Identifier(ident, args) = self.get_token_type(&variable) {
                        if args.len() != 0 {
                            return Exec::Err(self.error(
                                "variable identifiers cannot contain backtick arguments".to_owned(),
                            ));
                        }

                        if self.scope.is_var_defined(&ident) {
                            return Exec::Err(self.error(format!(
                                "attempt to redefine already defined variable `{}`",
                                ident
                            )));
                        }

                        let val = self.eval(&value, false)?;
                        self.scope.variables.insert_str(ident, RefCell::new(val));
                    }
                }

                Statement::Loop { condition, body } => {
                    let loop_ast = match body.as_ref() {
                        Statement::Block { statements } => statements,
                        _ => unreachable!(),
                    };

                    while self.eval(&condition, false)?.is_truthy() {
                        let mut loop_interpreter =
                            Interpreter::new(self.scope.child_scope(), &loop_ast);
                        loop_interpreter.line = self.line;
                        loop_interpreter.column = self.column;

                        let result = loop_interpreter.run();
                        self.line = loop_interpreter.line;
                        self.column = loop_interpreter.column;

                        if !loop_interpreter.variable_exports.is_empty()
                            || !loop_interpreter.function_exports.is_empty()
                        {
                            return Exec::Err(
                                self.error(
                                    "values/functions cannot be exported from within blocks"
                                        .to_string(),
                                ),
                            );
                        }

                        match result {
                            Exec::Break => break,
                            Exec::Continue => continue,
                            v => v?,
                        }
                    }
                }

                Statement::Conditional {
                    condition,
                    if_true,
                    if_false,
                } => {
                    let condition_val = self.eval(condition, false)?;

                    let body = if condition_val.is_truthy() {
                        if_true
                    } else if let Some(b) = if_false {
                        b
                    } else {
                        continue;
                    };

                    let body_ast = match *body.clone() {
                        Statement::Block { statements } => statements,
                        Statement::Conditional {
                            condition,
                            if_true,
                            if_false,
                        } => vec![Statement::Conditional {
                            condition,
                            if_true,
                            if_false,
                        }],
                        _ => unreachable!(),
                    };

                    let if_scope = self.scope.child_scope();
                    let mut if_interpreter = Interpreter::new(if_scope, &body_ast);
                    if_interpreter.line = self.line;
                    if_interpreter.column = self.column;

                    if_interpreter.run()?;
                    self.line = if_interpreter.line;
                    self.column = if_interpreter.column;

                    if !if_interpreter.variable_exports.is_empty()
                        || !if_interpreter.function_exports.is_empty()
                    {
                        return Exec::Err(self.error(
                            "values/functions cannot be exported from within blocks".to_string(),
                        ));
                    }
                }

                Statement::FunctionDefinition {
                    name: _,
                    parameters: _,
                    body: _,
                    export: _,
                } => (),
                Statement::Block { statements: _ } => (),
                Statement::Return { expression } => {
                    let value = match expression {
                        Some(e) => self.eval(e, false)?,
                        None => Value::Null,
                    };

                    return Exec::Return(value);
                }
                Statement::ExportVariable { name, value } => {
                    if let TokenType::Identifier(ident, args) = self.get_token_type(&name) {
                        if args.len() != 0 {
                            return Exec::Err(self.error(
                                "export identifiers cannot contain backtick arguments".to_owned(),
                            ));
                        }

                        let val = self.eval(&value, false)?;
                        self.variable_exports
                            .insert_str(ident.into(), RefCell::new(val));
                    }
                }
                Statement::Import { name } => {
                    if let TokenType::Identifier(ident, args) = self.get_token_type(&name) {
                        if args.len() != 0 {
                            return Exec::Err(self.error(
                                "import identifiers cannot contain backtick arguments".to_owned(),
                            ));
                        }

                        self.import_module(ident)?;
                    }
                }
                Statement::Break => return Exec::Break,
                Statement::Continue => return Exec::Continue,
                Statement::CallBuiltin { function: _ } => unreachable!(),
                Statement::StructDefinition { .. } => (),
            }
        }

        Exec::Ok
    }
}
