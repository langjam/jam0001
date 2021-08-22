use crate::values::CommentValue;

#[derive(Clone, Debug)]
pub enum ResolvedType {
    Unit,
    Number,
    Text,
    Comment(StructType),
    Struct(StructType),
}

impl ResolvedType {
    pub fn is_compatible_with(&self, _: &ResolvedType) -> bool {
        true // what could go wrong?
        /*
        match (self, other) {
            (ResolvedType::Unit, ResolvedType::Unit) => true,
            (ResolvedType::Number, ResolvedType::Number) => true,
            (ResolvedType::Text, ResolvedType::Text) => true,
            _ => false,
        }
        */
    }
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
