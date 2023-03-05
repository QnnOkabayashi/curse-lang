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
    InvalidToken(#[label("This token isn't valid")] usize),

    /// Unrecognized end-of-file
    #[diagnostic(help("Put more in the file?"))]
    UnrecognizedEOF(#[label("The files ends here...")] usize),

    /// Unrecognized token
    #[diagnostic(help("Use a recognized token instead."))]
    UnrecognizedToken(#[label("The token isn't recognized")] Range<usize>),

    /// Extra token
    #[diagnostic(help("Remove this token."))]
    ExtraToken(#[label("This token is extra")] Range<usize>),
}

impl std::error::Error for ParseError {}

type LalrParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, LexError>;

impl From<LalrParseError<'_>> for ParseError {
    fn from(value: LalrParseError<'_>) -> Self {
        use lalrpop_util::ParseError::*;

        match value {
            InvalidToken { location } => ParseError::InvalidToken(location),
            UnrecognizedEOF {
                location,
                expected: _,
            } => ParseError::UnrecognizedEOF(location),
            UnrecognizedToken {
                token: (start, _tok, end),
                expected: _,
            } => ParseError::UnrecognizedToken(start..end),
            ExtraToken {
                token: (start, _tok, end),
            } => ParseError::UnrecognizedToken(start..end),
            User { error: _ } => {
                todo!()
            }
        }
    }
}

// impl SourceErrors
