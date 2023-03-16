use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
pub enum EvalError<'input> {
    #[error("Type mismatch")]
    #[diagnostic(help("Wrong types"))]
    TypeMismatch,

    #[error("Identifier not defined: {var}")]
    #[diagnostic(help("Try a different name instead?"))]
    UnboundVariable {
        var: &'input str,

        #[label("here")]
        span: (usize, usize),
    },

    #[error("Bad pattern match")]
    #[diagnostic(help("idk"))]
    FailedPatternMatch,

    #[error("Parse int error")]
    #[diagnostic(help("Use a smaller integer"))]
    ParseInt(#[label("This integer is failed to parse into an i32")] SourceSpan),
}
