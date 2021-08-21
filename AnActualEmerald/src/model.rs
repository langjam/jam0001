#[derive(Debug)]
pub enum Term {
    Key(Keyword),
    String(String),
    Number(f32),
    Ident(String),
    Operation(Op)
}

#[derive(Debug)]
pub enum Keyword {
    Takes,
    Function,
    Call,
    Returns,
    Prints,
    Nothing,
    Equals,
    Is,
    And,
    Or,
    In,
    If,
    For,
    While,
    End,
}

#[derive(Debug)]
pub enum Op {
    Add(Box<Term>, Box<Term>),
    Subtract(Box<Term>, Box<Term>),
    Multiply(Box<Term>, Box<Term>),
    Divide(Box<Term>, Box<Term>),
}