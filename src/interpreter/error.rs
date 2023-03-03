use displaydoc::Display;

#[derive(Debug, Display)]
pub enum EvalError<'input> {
    /// Type mismatch
    TypeMismatch,

    /// Identifier not defined
    UnboundVariable(&'input str),
}

impl<'input> std::error::Error for EvalError<'input> {}
