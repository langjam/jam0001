use std::collections::HashMap;
use std::fmt;

use anyhow::{anyhow, bail, Result};

use crate::ast::{BinaryOperator, Expression, ScopeSpecifier, Statement, UnaryOperator};

pub type Scope = HashMap<String, Value>;
pub type Builtin = fn(&[Value]) -> Result<Value>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
}

impl Value {
    fn as_bool(&self) -> bool {
        match self {
            Value::Str(s) => !s.is_empty(),
            Value::Int(i) => *i != 0,
            Value::Bool(b) => *b,
        }
    }

    fn add(&self, other: Value) -> Result<Value> {
        match self {
            Value::Str(s) => Ok(Value::Str(format!("{}{}", s, other.to_string()))),
            Value::Int(i) => {
                if let Value::Int(other) = other {
                    Ok(Value::Int(i + other))
                } else {
                    Err(anyhow!("Type error"))
                }
            }
            _ => Err(anyhow!("Type error")),
        }
    }

    fn subtract(&self, other: Value) -> Result<Value> {
        if let Value::Int(a) = self {
            if let Value::Int(b) = other {
                return Ok(Value::Int(a - b));
            }
        }

        Err(anyhow!("Type error"))
    }

    fn multiply(&self, other: Value) -> Result<Value> {
        match self {
            Value::Str(s) => {
                if let Value::Int(other) = other {
                    if other < 0 {
                        Err(anyhow!("Negative repeat count"))
                    } else {
                        Ok(Value::Str(s.repeat(other as usize)))
                    }
                } else {
                    Err(anyhow!("Type error"))
                }
            }
            Value::Int(i) => {
                if let Value::Int(other) = other {
                    Ok(Value::Int(i * other))
                } else {
                    Err(anyhow!("Type error"))
                }
            }
            _ => Err(anyhow!("Type error")),
        }
    }

    fn divide(&self, other: Value) -> Result<Value> {
        if let Value::Int(a) = self {
            if let Value::Int(b) = other {
                return Ok(Value::Int(a / b));
            }
        }

        Err(anyhow!("Type error"))
    }

    fn modulo(&self, other: Value) -> Result<Value> {
        if let Value::Int(a) = self {
            if let Value::Int(b) = other {
                return Ok(Value::Int(a % b));
            }
        }

        Err(anyhow!("Type error"))
    }

    fn and(&self, other: Value) -> Result<Value> {
        Ok(Value::Bool(self.as_bool() && other.as_bool()))
    }

    fn or(&self, other: Value) -> Result<Value> {
        Ok(Value::Bool(self.as_bool() || other.as_bool()))
    }

    fn not(&self) -> Result<Value> {
        Ok(Value::Bool(!self.as_bool()))
    }

    fn negate(&self) -> Result<Value> {
        if let Value::Int(i) = self {
            return Ok(Value::Int(-i));
        }

        Err(anyhow!("Type error"))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{}", s),
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct FunctionContext {
    parameters: Vec<String>,
    statements: Vec<Statement>,
    static_variables: Scope,
}

#[derive(Clone)]
pub struct Context {
    global_variables: Scope,
    function_contexts: HashMap<String, FunctionContext>,
    builtins: HashMap<String, Builtin>,
}

impl Context {
    pub fn new(builtins: HashMap<String, Builtin>) -> Self {
        Self {
            global_variables: Default::default(),
            function_contexts: Default::default(),
            builtins,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementResult {
    Continue,
    Goto(String),
    Return(Value),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionResult {
    Value(Value),
    Goto(String),
}

impl Context {
    pub fn eval(&mut self, statements: &[Statement]) -> Result<StatementResult> {
        let mut local_variables: Scope = Default::default();
        self.eval_statements(statements, &mut local_variables, &None)
    }

    fn eval_statements(
        &mut self,
        statements: &[Statement],
        local_variables: &mut Scope,
        function: &Option<String>,
    ) -> Result<StatementResult> {
        for statement in statements {
            match self.eval_statement(statement, local_variables, function) {
                Ok(StatementResult::Continue) => (),
                exit => {
                    return exit;
                }
            }
        }

        Ok(StatementResult::Continue)
    }

    fn eval_statement(
        &mut self,
        statement: &Statement,
        local_variables: &mut Scope,
        function: &Option<String>,
    ) -> Result<StatementResult> {
        match statement {
            Statement::Goto(expression) => {
                match self.eval_expression(&*expression, local_variables, function)? {
                    ExpressionResult::Value(Value::Str(s)) => return Ok(StatementResult::Goto(s)),
                    ExpressionResult::Goto(s) => {
                        return Ok(StatementResult::Goto(s));
                    }
                    _ => {
                        bail!("Attempted to jump to a non-string label");
                    }
                }
            }
            Statement::Declare {
                scope: ScopeSpecifier::Global,
                name,
                expression,
            } => {
                if self.global_variables.contains_key(name) {
                    return Ok(StatementResult::Continue);
                }

                match self.eval_expression(expression, local_variables, function)? {
                    ExpressionResult::Value(v) => {
                        self.global_variables.insert(name.clone(), v);
                    }
                    ExpressionResult::Goto(s) => {
                        return Ok(StatementResult::Goto(s));
                    }
                }
            }
            Statement::Declare {
                scope: ScopeSpecifier::Static,
                name,
                expression,
            } => {
                if function.is_none() {
                    bail!("Attempted to define a static variable outside of a function context");
                }

                if function
                    .as_ref()
                    .and_then(|f| self.function_contexts.get(f))
                    .map_or(false, |ctx| ctx.static_variables.contains_key(name))
                {
                    return Ok(StatementResult::Continue);
                }

                match self.eval_expression(expression, local_variables, function)? {
                    ExpressionResult::Value(v) => {
                        function.as_ref().and_then(|f| {
                            self.function_contexts
                                .get_mut(f)
                                .unwrap()
                                .static_variables
                                .insert(name.clone(), v)
                        });
                    }
                    ExpressionResult::Goto(s) => {
                        return Ok(StatementResult::Goto(s));
                    }
                }
            }
            Statement::Declare {
                scope: ScopeSpecifier::Local,
                name,
                expression,
            } => {
                if local_variables.contains_key(name) {
                    bail!("Attemped to redefine local variable");
                }

                match self.eval_expression(expression, local_variables, function)? {
                    ExpressionResult::Value(v) => {
                        local_variables.insert(name.clone(), v);
                    }
                    ExpressionResult::Goto(s) => {
                        return Ok(StatementResult::Goto(s));
                    }
                }
            }
            Statement::Assignment { name, expression } => {
                // TODO: If this expression contains a function call which has side effects,
                // but the assignment fails, this will still perform the side-effects.
                let value = match self.eval_expression(expression, local_variables, function)? {
                    ExpressionResult::Value(v) => v,
                    ExpressionResult::Goto(s) => {
                        return Ok(StatementResult::Goto(s));
                    }
                };
                if let Some(v) = local_variables.get_mut(name) {
                    *v = value;
                } else if let Some(v) = function.as_ref().and_then(|f| {
                    self.function_contexts
                        .get_mut(f)
                        .and_then(|ctx| ctx.static_variables.get_mut(name))
                }) {
                    *v = value;
                } else if let Some(v) = self.global_variables.get_mut(name) {
                    *v = value;
                } else {
                    bail!("Attempted to assign to an undeclared variable");
                }
            }
            Statement::If {
                conditional,
                statements,
                else_statements,
            } => {
                let conditional =
                    match self.eval_expression(conditional, local_variables, function)? {
                        ExpressionResult::Value(v) => v.as_bool(),
                        ExpressionResult::Goto(s) => {
                            return Ok(StatementResult::Goto(s));
                        }
                    };

                if conditional {
                    return self.eval_statements(statements, local_variables, function);
                } else if let Some(else_statements) = else_statements {
                    return self.eval_statements(else_statements, local_variables, function);
                }
            }
            Statement::FunctionDefinition {
                name,
                parameters,
                statements,
            } => {
                if function.is_some() {
                    bail!("Attempted to define a function within a function context");
                }

                self.function_contexts.insert(
                    name.clone(),
                    FunctionContext {
                        parameters: parameters.clone(),
                        statements: statements.clone(),
                        ..Default::default()
                    },
                );
            }
            Statement::Expression(expression) => {
                if let ExpressionResult::Goto(s) =
                    self.eval_expression(expression, local_variables, function)?
                {
                    return Ok(StatementResult::Goto(s));
                }
            }
            Statement::Return(expression) => {
                match self.eval_expression(expression, local_variables, function)? {
                    ExpressionResult::Value(v) => return Ok(StatementResult::Return(v)),
                    ExpressionResult::Goto(s) => {
                        return Ok(StatementResult::Goto(s));
                    }
                }
            }
        }

        Ok(StatementResult::Continue)
    }

    fn eval_expression(
        &mut self,
        expression: &Expression,
        local_variables: &mut Scope,
        function: &Option<String>,
    ) -> Result<ExpressionResult> {
        match expression {
            Expression::Str(s) => Ok(ExpressionResult::Value(Value::Str(s.clone()))),
            Expression::Int(i) => Ok(ExpressionResult::Value(Value::Int(*i))),
            Expression::Bool(b) => Ok(ExpressionResult::Value(Value::Bool(*b))),
            Expression::Ident(name) => {
                if let Some(v) = local_variables.get(name) {
                    Ok(ExpressionResult::Value(v.clone()))
                } else if let Some(v) = function.as_ref().and_then(|f| {
                    self.function_contexts
                        .get(f)
                        .and_then(|ctx| ctx.static_variables.get(name))
                }) {
                    Ok(ExpressionResult::Value(v.clone()))
                } else if let Some(v) = self.global_variables.get(name) {
                    Ok(ExpressionResult::Value(v.clone()))
                } else {
                    Err(anyhow!("Attempted to access an undeclared variable"))
                }
            }
            Expression::BinaryExpression {
                operator,
                left,
                right,
            } => {
                let lhs = match self.eval_expression(left, local_variables, function)? {
                    ExpressionResult::Value(v) => v,
                    goto => {
                        return Ok(goto);
                    }
                };
                let rhs = match self.eval_expression(right, local_variables, function)? {
                    ExpressionResult::Value(v) => v,
                    goto => {
                        return Ok(goto);
                    }
                };

                match operator {
                    BinaryOperator::Add => Ok(ExpressionResult::Value(lhs.add(rhs)?)),
                    BinaryOperator::Subtract => Ok(ExpressionResult::Value(lhs.subtract(rhs)?)),
                    BinaryOperator::Multiply => Ok(ExpressionResult::Value(lhs.multiply(rhs)?)),
                    BinaryOperator::Divide => Ok(ExpressionResult::Value(lhs.divide(rhs)?)),
                    BinaryOperator::Modulo => Ok(ExpressionResult::Value(lhs.modulo(rhs)?)),
                    BinaryOperator::And => Ok(ExpressionResult::Value(lhs.and(rhs)?)),
                    BinaryOperator::Or => Ok(ExpressionResult::Value(lhs.or(rhs)?)),
                    BinaryOperator::Equals => Ok(ExpressionResult::Value(Value::Bool(lhs == rhs))),
                    BinaryOperator::NotEquals => {
                        Ok(ExpressionResult::Value(Value::Bool(lhs != rhs)))
                    }
                    BinaryOperator::GreaterThanEquals => {
                        Ok(ExpressionResult::Value(Value::Bool(lhs >= rhs)))
                    }
                    BinaryOperator::LessThanEquals => {
                        Ok(ExpressionResult::Value(Value::Bool(lhs <= rhs)))
                    }
                    BinaryOperator::GreaterThan => {
                        Ok(ExpressionResult::Value(Value::Bool(lhs > rhs)))
                    }
                    BinaryOperator::LessThan => Ok(ExpressionResult::Value(Value::Bool(lhs < rhs))),
                }
            }
            Expression::UnaryExpression {
                operator,
                expression,
            } => {
                let expression =
                    match self.eval_expression(expression, local_variables, function)? {
                        ExpressionResult::Value(v) => v,
                        goto => {
                            return Ok(goto);
                        }
                    };

                match operator {
                    UnaryOperator::Not => Ok(ExpressionResult::Value(expression.not()?)),
                    UnaryOperator::Negate => Ok(ExpressionResult::Value(expression.negate()?)),
                }
            }
            Expression::FunctionCall { name, arguments } => {
                let arguments = arguments
                    .iter()
                    .map(|arg| self.eval_expression(&arg, local_variables, function))
                    .collect::<Result<Vec<_>>>()?;

                // Check for early goto
                for arg in arguments.iter() {
                    if let ExpressionResult::Goto(s) = arg {
                        return Ok(ExpressionResult::Goto(s.clone()));
                    }
                }

                let arguments: Vec<_> = arguments
                    .iter()
                    .map(|arg| match arg {
                        ExpressionResult::Value(v) => v.clone(),
                        _ => panic!("Encountered unexpected goto"),
                    })
                    .collect();

                if let Some(builtin) = self.builtins.get(name) {
                    Ok(ExpressionResult::Value(builtin(&arguments)?))
                } else {
                    let (statements, parameters) = self
                        .function_contexts
                        .get(name)
                        .map(|ctx| (ctx.statements.clone(), ctx.parameters.clone()))
                        .ok_or_else(|| anyhow!("Attempted to call undefined function"))?;

                    if arguments.len() != parameters.len() {
                        bail!("Incorrect number of parameters to function");
                    }

                    let mut new_scope = parameters
                        .into_iter()
                        .zip(arguments.into_iter())
                        .collect::<Scope>();

                    match self.eval_statements(&statements, &mut new_scope, &Some(name.clone()))? {
                        StatementResult::Goto(s) => Ok(ExpressionResult::Goto(s)),
                        StatementResult::Return(v) => Ok(ExpressionResult::Value(v)),
                        // TOOD: We don't have a void type, so if we don't return from a function make
                        // this equivalent to return false
                        _ => Ok(ExpressionResult::Value(Value::Bool(false))),
                    }
                }
            }
        }
    }
}
