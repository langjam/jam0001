use crate::expression::Expression;

pub type Program = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    FileHeader(FileHeader),
    DefinitionHeader(DefinitionHeader),
    FunctionDefinition(FunctionDefinition),
    Expression(Expression),
    If(If),
    While(While),
    Var(Var),
    Const(Const),
    ForIn(ForIn),
}

#[derive(Debug, Clone)]
pub struct ForIn {
    pub counter: Option<String>,
    pub iterator: String,
    pub iterable: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub init: Expression,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub init: Expression,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub then: Vec<Statement>,
    pub otherwise: Vec<Statement>
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub then: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FileHeader {
    pub name: String,
    pub description: Option<String>,
    pub author: Option<String>,
    pub version: Option<String>,
}

#[derive(Debug, Clone)]
pub struct DefinitionHeader {
    pub identifier: String,
    pub params: Vec<(String, Option<String>)>,
    pub r#type: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub body: Vec<Statement>,
}