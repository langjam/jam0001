#[derive(Debug, Clone)]
pub struct Comment {
    pub lines: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum TypeAst {
    Named(String),
    Struct(StructType),
}

#[derive(Debug, Clone)]
pub struct FieldType {
    pub name: String,
    pub ty: TypeAst,
    pub comment: Comment,
}

#[derive(Debug, Clone)]
pub struct StructTypeData {
    pub name: Option<String>,
    pub fields: Vec<FieldType>,
    pub comment: Comment,
}

#[derive(Debug, Clone)]
pub struct StructType(std::rc::Rc<StructTypeData>);

impl std::ops::Deref for StructType {
    type Target = StructTypeData;

    fn deref(&self) -> &StructTypeData {
        &self.0
    }
}

impl From<StructTypeData> for StructType {
    fn from(data: StructTypeData) -> Self {
        StructType(std::rc::Rc::new(data))
    }
}

#[derive(Debug, Clone)]
pub struct FieldValue {
    pub name: String,
    pub value: ValueAst,
    pub comment: Comment,
}

#[derive(Debug, Clone)]
pub struct StructValueData {
    pub name: Option<String>,
    pub fields: Vec<FieldValue>,
    pub comment: Comment,
}

#[derive(Debug, Clone)]
pub struct StructValue(std::rc::Rc<StructValueData>);

impl StructValue {
    pub fn inner(&self) -> &StructValueData {
        &self.0
    }
}

impl std::ops::Deref for StructValue {
    type Target = StructValueData;

    fn deref(&self) -> &StructValueData {
        &self.0
    }
}

impl From<StructValueData> for StructValue {
    fn from(data: StructValueData) -> Self {
        StructValue(std::rc::Rc::new(data))
    }
}

#[derive(Debug, Clone)]
pub enum ValueAst {
    Unit,
    Number(i64),
    Text(String),
    Struct(StructValue),
    Reference(String),
    CommentAccess(Box<ValueAst>, String),
    FieldAccess(Box<ValueAst>, String),
    CommentGet(Box<ValueAst>),
    FieldCommentGet(Box<ValueAst>, String),
    Add(Box<ValueAst>, Box<ValueAst>),
    // TODO: expressions?
}

#[derive(Debug, Clone)]
pub enum Ast {
    TypeDef(StructType),
    ValueDef(String, ValueAst),
}

#[derive(Debug, Clone)]
pub struct Script {
    pub name: String,
    pub statements: Vec<Ast>,
}
