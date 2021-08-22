use std::collections::HashMap;

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum Expression {
    BinaryOp {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOp {
        operator: Token,
        right: Box<Expression>,
    },
    Literal {
        value: Token,
    },
    Variable {
        name: Token,
    },
    ArrayLiteral {
        elements: Vec<Expression>,
    },
    ArrayRef {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    StructLiteral {
        struct_type: String,
        fields: HashMap<String, Expression>,
    },
    StructFieldRef {
        target: Box<Expression>,
        field_name: String,
    },
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression {
        expr: Expression,
    },
    Block {
        statements: Vec<Statement>,
    },
    Definition {
        variable: Token,
        value: Expression,
    },
    Conditional {
        condition: Expression,
        if_true: Box<Statement>,
        if_false: Option<Box<Statement>>,
    },
    Loop {
        condition: Expression,
        body: Box<Statement>,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<String>,
        body: Box<Statement>,
        export: bool
    },
    Return {
        expression: Option<Expression>,
    },
    Break,
    Continue,
    ExportVariable {
        name: Token,
        value: Expression,
    },
    Import {
        name: Token,
    },
    CallBuiltin {
        function: crate::builtins::BuiltinFunction,
    },
    StructDefinition {
        struct_type: String,
        fields: Vec<String>,
        export: bool
    },
}
