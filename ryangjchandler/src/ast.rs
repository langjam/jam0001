pub type Program = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    FileHeader(FileHeader),
    Empty
}

#[derive(Debug, Clone)]
pub struct FileHeader {
    pub name: String,
    // description: Option<String>,
    // author: Option<String>
}