use crate::ast::Statement;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub body: Vec<Statement>,
}