use crate::lex::{LexError, Token};
use displaydoc::Display;
use miette::{Diagnostic, NamedSource};
use std::ops::Range;

#[derive(Debug, Diagnostic, Display)]
#[displaydoc("A parsing error occurred.")]
pub struct SourceErrors {
    #[source_code]
    pub source: NamedSource,

    #[related]
    pub errors: Vec<ParseError>,
}

impl std::error::Error for SourceErrors {}

#[allow(dead_code)]
#[derive(Debug, Diagnostic, Display)]
pub enum ParseError {
    /// Invalid token
    #[diagnostic(help("Try using a valid token instead."))]
    InvalidToken {
        #[label("This token isn't valid")]
        location: usize,
    },

    /// Unrecognized end-of-file
    #[diagnostic(help("Put more in the file?"))]
    UnrecognizedEOF {
        #[label("The files ends here...")]
        location: usize,
    },

    /// Unrecognized token
    #[diagnostic(help("Use an expected token instead: {expected:?}"))]
    UnrecognizedToken {
        expected: Vec<String>,

        #[label("The token isn't recognized")]
        span: Range<usize>,
    },

    /// Extra token
    #[diagnostic(help("Remove this token."))]
    ExtraToken {
        #[label("This token is extra")]
        span: Range<usize>,
    },

    /// Lexing error
    #[diagnostic(help("Fix your code"))]
    Lex {
        #[label("This isn't recognized by the lexer")]
        span: Range<usize>,
    },
}

impl std::error::Error for ParseError {}

type LalrParseError<'input> = lalrpop_util::ParseError<usize, Token<'input>, LexError>;

impl From<LalrParseError<'_>> for ParseError {
    fn from(value: LalrParseError<'_>) -> Self {
        use lalrpop_util::ParseError::*;

        match value {
            InvalidToken { location } => ParseError::InvalidToken { location },
            UnrecognizedEOF { location, .. } => ParseError::UnrecognizedEOF { location },
            UnrecognizedToken {
                token: (start, _, end),
                expected,
            } => ParseError::UnrecognizedToken {
                expected,
                span: start..end,
            },
            ExtraToken {
                token: (start, _, end),
            } => ParseError::ExtraToken { span: start..end },
            User { error } => ParseError::Lex {
                span: error.span.into(),
            },
        }
    }
}

// impl SourceErrors
