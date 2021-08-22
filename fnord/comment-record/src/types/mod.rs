use crate::values::CommentValue;

#[derive(Clone, Debug)]
pub enum ResolvedType {
    Unit,
    Number,
    Text,
    //Comment(StructType),
    Struct(StructType),
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
pub struct FieldType {
    pub name: String,
    pub ty: ResolvedType,
    pub comment: CommentValue,
}

#[derive(Debug, Clone)]
pub struct StructTypeData {
    pub name: Option<String>,
    pub fields: Vec<FieldType>,
    pub comment: CommentValue,
}
