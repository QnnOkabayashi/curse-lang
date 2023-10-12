use curse_span::Span;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
pub enum EvalError {
    #[error("Missing `main` function")]
    MissingMain,

    #[error("Unbound literal: {literal}")]
    UnboundVariable { literal: String, span: Span },

    #[error("Failed pattern match")]
    FailedPatternMatch,

    #[error("{0}")]
    Match(MatchError),

    #[error("Pattern match refuted")]
    PatternMatchRefuted,

    #[error("Type mismatch (prolly compiler bug)")]
    TypeMismatch(String),

    #[error("Attempted to divide by 0")]
    DivByZero,

    #[error("Debug: {0}")]
    Debug(i32),
}

#[derive(Debug, Diagnostic, Error)]
pub enum MatchError {
    #[error("cannot have an integer literal as a field pattern")]
    #[diagnostic(help("remove this field"))]
    IntLiteralField(#[label("here")] SourceSpan),

    #[error("cannot have an boolean literal as a field pattern")]
    #[diagnostic(help("remove this field"))]
    BoolLiteralField(#[label("here")] SourceSpan),

    #[error("record field pattern without value")]
    #[diagnostic(help("inline these fields"))]
    RecordFieldPatWithoutValue(#[label("here")] SourceSpan),
}
