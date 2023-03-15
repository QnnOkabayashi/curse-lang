use displaydoc::Display;
use miette::{Diagnostic, SourceSpan};

#[derive(Debug, Diagnostic, Display)]
pub enum EvalError<'input> {
    /// Type mismatch
    #[diagnostic(help("Wrong types"))]
    TypeMismatch,

    /// Identifier not defined: {0}
    #[diagnostic(help("Unbound variable"))]
    UnboundVariable(&'input str),

    /// Bad Pattern match
    #[diagnostic(help("idk"))]
    FailedPatternMatch,

    /// Bad Pattern match
    #[diagnostic(help("Use a smaller integer"))]
    ParseInt(#[label("This integer is failed to parse into an i32")] SourceSpan),

    /// A lexing error occurred
    #[diagnostic(help("Fix your type"))]
    LexError,
}

impl std::error::Error for EvalError<'_> {}
