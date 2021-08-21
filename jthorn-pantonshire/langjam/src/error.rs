use crate::parser::Span;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub struct RuntimeError {
    pub span: Span,
    pub cause: RuntimeErrorCause,
}

pub enum RuntimeErrorCause {
    MissingVariable,
    MissingFunction,
    TypeError,
    Immutable,
}

impl RuntimeError {
    pub const fn new(span: Span, cause: RuntimeErrorCause) -> Self {
        Self {
            span,
            cause,
        }
    }
}

impl RuntimeErrorCause {
    pub const fn error(self, span: Span) -> RuntimeError {
        RuntimeError {
            span,
            cause: self,
        }
    }
}
