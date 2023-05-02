use crate::lexer::{LexError, Token};
use lalrpop_util::ParseError;
use miette::{Diagnostic, NamedSource};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
#[error("A parsing error occurred.")]
pub struct SourceErrors {
    #[source_code]
    pub code: NamedSource,

    #[related]
    pub errors: Vec<Error>,
}

#[derive(Debug, Diagnostic, Error)]
pub enum Error {
    #[error("Closure application missing right-hand side.")]
    #[diagnostic(help("If you're trying to pass a value to a function, use the `in` function, or wrap the closure in `()` and add the right-hand side argument."))]
    ClosureApplMissingRhs {
        /// The first token of the closure
        #[label("Cannot call this closure without a right-hand side")]
        location: usize,
    },

    #[error("Invalid token")]
    #[diagnostic(help("Try using a valid token instead."))]
    InvalidToken {
        #[label("This token isn't valid")]
        location: usize,
    },

    #[error("Unrecognized end-of-file")]
    #[diagnostic(help("Put more in the file?"))]
    UnrecognizedEOF {
        #[label("The files ends here...")]
        location: usize,
    },

    #[error("Unrecognized token")]
    #[diagnostic(help("Use an expected token instead: {expected:?}"))]
    UnrecognizedToken {
        expected: Vec<String>,

        #[label("The token isn't recognized")]
        span: (usize, usize),
    },

    #[error("Extra token")]
    #[diagnostic(help("Remove this token."))]
    ExtraToken {
        #[label("This token is extra")]
        span: (usize, usize),
    },

    #[error("Lexing error")]
    #[diagnostic(help("Fix your code"))]
    Lex {
        #[label("This isn't recognized by the lexer")]
        span: (usize, usize),
    },
}

type LalrParseError<'input> = ParseError<usize, Token<'input>, LexError>;

impl From<LalrParseError<'_>> for Error {
    fn from(value: LalrParseError<'_>) -> Self {
        use ParseError::*;

        match value {
            InvalidToken { location } => Error::InvalidToken { location },
            UnrecognizedEOF { location, .. } => Error::UnrecognizedEOF { location },
            UnrecognizedToken {
                token: (_, token, _),
                expected,
            } => Error::UnrecognizedToken {
                expected,
                span: token.span(),
            },
            ExtraToken {
                token: (_, token, _),
            } => Error::ExtraToken { span: token.span() },
            User {
                error: LexError { span },
            } => Error::Lex { span },
        }
    }
}

// impl SourceErrors
