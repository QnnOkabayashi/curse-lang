use crate::lexer::{LexError, Token};
use displaydoc::Display;
use lalrpop_util::ParseError;
use miette::{Diagnostic, NamedSource};

#[derive(Debug, Diagnostic, Display)]
#[displaydoc("A parsing error occurred.")]
pub struct SourceErrors {
    #[source_code]
    pub source: NamedSource,

    #[related]
    pub errors: Vec<Error>,
}

impl std::error::Error for SourceErrors {}

#[allow(dead_code)]
#[derive(Debug, Diagnostic, Display)]
pub enum Error {
    #[diagnostic(help("Try using a valid token instead."))]
    #[displaydoc("Invalid token")]
    InvalidToken {
        #[label("This token isn't valid")]
        location: usize,
    },

    #[displaydoc("Unrecognized end-of-file")]
    #[diagnostic(help("Put more in the file?"))]
    UnrecognizedEOF {
        #[label("The files ends here...")]
        location: usize,
    },

    #[displaydoc("Unrecognized token")]
    #[diagnostic(help("Use an expected token instead: {expected:?}"))]
    UnrecognizedToken {
        expected: Vec<String>,

        #[label("The token isn't recognized")]
        span: (usize, usize),
    },

    #[displaydoc("Extra token")]
    #[diagnostic(help("Remove this token."))]
    ExtraToken {
        #[label("This token is extra")]
        span: (usize, usize),
    },

    #[displaydoc("Lexing error")]
    #[diagnostic(help("Fix your code"))]
    Lex {
        #[label("This isn't recognized by the lexer")]
        span: (usize, usize),
    },
}

impl std::error::Error for Error {}

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
