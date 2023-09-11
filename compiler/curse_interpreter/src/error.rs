use curse_span::Span;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
pub enum EvalError {
    #[error("Missing `main` function")]
    MissingMain,

    #[error("Unbound literal: {literal}")]
    UnboundVariable { literal: String, span: Span },

    #[error("Failed pattern match")]
    FailedPatternMatch,

    #[error("Pattern match refuted")]
    PatternMatchRefuted,

    #[error("Type mismatch (prolly compiler bug)")]
    TypeMismatch,

    #[error("Missing field in record")]
    MissingField,
}
