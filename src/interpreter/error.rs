use displaydoc::Display;
use miette::Diagnostic;

#[derive(Debug, Diagnostic, Display)]
pub enum EvalError<'input> {
    /// Type mismatch
    #[diagnostic(help("Wrong types"))]
    TypeMismatch,

    /// Identifier not defined
    #[diagnostic(help("Unbound variable"))]
    UnboundVariable(&'input str),

    /// Bad Pattern match
    #[diagnostic(help("idk"))]
    FailedPatternMatch,
}

impl<'input> std::error::Error for EvalError<'input> {}
