use crate::parser::Span;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub struct RuntimeError {
    pub span: Span,
    pub cause: RuntimeErrorCause,
}

pub enum RuntimeErrorCause {
    MissingVariable,
    TypeError,
}

impl RuntimeError {
    pub const fn new(span: Span, cause: RuntimeErrorCause) -> Self {
        Self {
            span,
            cause,
        }
    }
}
