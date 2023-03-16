use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
pub enum EvalError<'input> {
    #[error("Type mismatch")]
    #[diagnostic(help("Wrong types"))]
    TypeMismatch,

    #[error("Identifier not defined: {0}")]
    #[diagnostic(help("Unbound variable"))]
    UnboundVariable(&'input str),

    #[error("Bad pattern match")]
    #[diagnostic(help("idk"))]
    FailedPatternMatch,

    #[error("Parse int error")]
    #[diagnostic(help("Use a smaller integer"))]
    ParseInt(#[label("This integer is failed to parse into an i32")] SourceSpan),
}
