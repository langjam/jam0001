#[derive(Debug, Clone)]
pub enum Term {
    Key(Keyword),
    String(String),
    Number(f32),
    Ident(String),
    Operation(Op),
    Bool(bool),
    List(Vec<Box<Term>>),
    Newline,
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Ident(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            o => write!(f, "{:?}", o),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Takes(Vec<String>),
    With(Box<Term>),
    Function,
    Call,
    Returns,
    Prints,
    Nothing,
    Equals(String),
    Is,
    And,
    Or,
    In,
    If, // TODO: Implement control flow
    For,
    While,
    End,
    Waste, // Where the useless keywords go
}

#[derive(Debug, Clone)]
pub enum Op {
    Add(Box<Term>, Box<Term>),
    Subtract(Box<Term>, Box<Term>),
    Multiply(Box<Term>, Box<Term>),
    Divide(Box<Term>, Box<Term>),
}
