use crate::lexer::{LexError, Token};
use lalrpop_util::ParseError;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
pub enum Error {
    #[error("Closure application missing right-hand side.")]
    #[diagnostic(help("If you're trying to pass a value to a function, use the `in` function, or wrap the closure in `()` and add the right-hand side argument."))]
    ClosureApplMissingRhs {
        /// The first token of the closure
        #[label("Cannot call this closure without a right-hand side")]
        location: SourceSpan,
    },

    #[error("Invalid token")]
    #[diagnostic(help("Try using a valid token instead."))]
    InvalidToken {
        #[label("This token isn't valid")]
        location: SourceSpan,
    },

    #[error("Unrecognized end-of-file")]
    #[diagnostic(help("Put more in the file?"))]
    UnrecognizedEOF {
        #[label("The files ends here...")]
        location: SourceSpan,
    },

    #[error("Unrecognized token")]
    #[diagnostic(help("Use an expected token instead: {expected:?}"))]
    UnrecognizedToken {
        expected: Vec<String>,

        #[label("The token isn't recognized")]
        span: SourceSpan,
    },

    #[error("Extra token")]
    #[diagnostic(help("Remove this token."))]
    ExtraToken {
        #[label("This token is extra")]
        span: SourceSpan,
    },

    #[error("Lexing error")]
    #[diagnostic(help("Fix your code."))]
    UnknownSeq(#[label("This isn't recognized by the lexer")] SourceSpan),

    #[error("Invalid identifier")]
    #[diagnostic(help("Try making this a valid unicode identifier."))]
    InvalidIdent(#[label("This isn't a valid unicode identifier")] SourceSpan),

    #[error("Invalid integer")]
    #[diagnostic(help("Try making this a valid integer."))]
    InvalidInteger(#[label("This isn't a valid integer")] SourceSpan),
}

type LalrParseError<'input> = ParseError<usize, Token<'input>, LexError>;

impl From<LalrParseError<'_>> for Error {
    fn from(value: LalrParseError<'_>) -> Self {
        use ParseError::*;

        match value {
            InvalidToken { location } => Error::InvalidToken {
                location: location.into(),
            },
            UnrecognizedEOF { location, .. } => Error::UnrecognizedEOF {
                location: location.into(),
            },
            UnrecognizedToken {
                token: (_, token, _),
                mut expected,
            } => {
                for s in expected.iter_mut() {
                    s.retain(|ch| ch != '\"');
                }
                Error::UnrecognizedToken {
                    expected,
                    span: token.span().start_len().into(),
                }
            }
            ExtraToken {
                token: (_, token, _),
            } => Error::ExtraToken {
                span: token.span().start_len().into(),
            },
            User { error } => match error {
                LexError::UnknownSeq(span) => Error::UnknownSeq(span.start_len().into()),
                LexError::InvalidIdent(span) => Error::InvalidIdent(span.start_len().into()),
                LexError::InvalidInteger(span) => Error::InvalidInteger(span.start_len().into()),
            },
        }
    }
}
