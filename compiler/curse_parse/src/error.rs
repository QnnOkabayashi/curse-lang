use crate::lexer::{LexError, Token};
use displaydoc::Display;
use miette::{Diagnostic, NamedSource, SourceSpan};

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
        span: SourceSpan,
    },

    #[displaydoc("Extra token")]
    #[diagnostic(help("Remove this token."))]
    ExtraToken {
        #[label("This token is extra")]
        span: SourceSpan,
    },

    #[displaydoc("Lexing error")]
    #[diagnostic(help("Fix your code"))]
    Lex {
        #[label("This isn't recognized by the lexer")]
        span: SourceSpan,
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
                token: (_, token, _),
                expected,
            } => ParseError::UnrecognizedToken {
                expected,
                span: token.span().into(),
            },
            ExtraToken {
                token: (_, token, _),
            } => ParseError::ExtraToken {
                span: token.span().into(),
            },
            User {
                error: LexError { span },
            } => ParseError::Lex { span: span.into() },
        }
    }
}

// impl SourceErrors
