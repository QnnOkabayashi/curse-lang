use curse_span::Span;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
pub enum EvalError {
    #[error("Missing `main` function")]
    #[diagnostic(help("Add a `main` function"))]
    MissingMain,

    #[error("Unbound literal: {literal}")]
    UnboundVariable { literal: String, span: Span },
}
