use logos::Logos;
use std::{fmt, ops::Range};

type Loc = usize;

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: Loc,
    end: Loc,
}

impl From<Range<Loc>> for Span {
    fn from(Range { start, end }: Range<Loc>) -> Self {
        Span { start, end }
    }
}

impl From<Span> for Range<Loc> {
    fn from(Span { start, end }: Span) -> Self {
        start..end
    }
}

impl From<&Span> for Range<Loc> {
    fn from(Span { start, end }: &Span) -> Self {
        *start..*end
    }
}

macro_rules! declare_tokens {
    ($($(#[$attr:meta])* $tok:literal => $name:ident,)*) => {

        #[derive(Clone, Debug, Logos)]
        #[logos(subpattern ws = r"[ \t\v\r\n\f]")]
        enum LogosToken {
            #[regex("[_a-zA-Z][_a-zA-Z0-9]*")]
            Ident,
            #[regex("[0-9]+")]
            Integer,

            $(
                #[token($tok)]
                $name,
            )*

            #[regex(r"(?&ws)+", logos::skip)]
            Whitespace,
            #[regex("//[^\r\n]*", logos::skip)]
            Comment,
            #[error]
            Unknown,
        }

        pub mod tok {
            use super::Span;

            #[derive(Copy, Clone, Debug)]
            pub struct Ident<'input> {
                pub span: Span,
                pub literal: &'input str,
            }

            #[derive(Copy, Clone, Debug)]
            pub struct Integer<'input> {
                pub span: Span,
                pub literal: &'input str,
            }

            $(
                $(#[$attr])*
                #[derive(Copy, Clone, Debug)]
                pub struct $name {
                    pub span: Span,
                }
            )*
        }

        #[derive(Copy, Clone, Debug)]
        pub enum Token<'input> {
            Ident(tok::Ident<'input>),
            Integer(tok::Integer<'input>),
            $(
                $(#[$attr])*
                $name(tok::$name),
            )*
        }

        impl Token<'_> {
            pub fn span(&self) -> Span {
                match self {
                    Token::Ident(tok) => tok.span,
                    Token::Integer(tok) => tok.span,
                    $(
                        Token::$name(tok) => tok.span,
                    )*
                }
            }
        }

        impl fmt::Display for Token<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(match self {
                    Token::Ident(tok) => tok.literal,
                    Token::Integer(tok) => tok.literal,
                    $(
                        Token::$name(_) => $tok,
                    )*
                })
            }
        }

        impl<'input> Iterator for Lexer<'input> {
            type Item = Result<(Loc, Token<'input>, Loc), LexError>;

            fn next(&mut self) -> Option<Self::Item> {
                let token = match self.lex.next()? {
                    LogosToken::Ident => Token::Ident(tok::Ident {
                        span: self.lex.span().into(),
                        literal: self.lex.slice(),
                    }),
                    LogosToken::Integer => Token::Integer(tok::Integer {
                        span: self.lex.span().into(),
                        literal: self.lex.slice(),
                    }),
                    $(
                        LogosToken::$name => Token::$name(tok::$name {
                            span: self.lex.span().into(),
                        }),
                    )*
                    LogosToken::Unknown => return Some(Err(LexError {
                        span: self.lex.span().into(),
                    })),
                    _ => unreachable!("remaining patterns are skipped"),
                };

                let Range { start, end } = self.lex.span();
                Some(Ok((start, token, end)))
            }
        }
    }
}

// Make sure to update the `extern` block in `src/grammar.lalrpop`
// if you make any changes here.
declare_tokens! {
    ":" => Colon,
    "," => Comma,
    "(" => LParen,
    ")" => RParen,
    "+" => Plus,
    "-" => Minus,
    "*" => Star,
    ".." => DotDot,
    ";" => Semi,
    "%" => Percent,
    "/" => Slash,
    "|" => Pipe,
    "fn" => Fn,
    "else" => Else,
    "->" => Arrow,

    "=" => Equal,
    "<" => Less,
    ">" => Greater,
    "<=" => LessEqual,
    ">=" => GreaterEqual,
}

#[derive(Copy, Clone, Debug)]
pub struct LexError {
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Lexer<'input> {
    lex: logos::Lexer<'input, LogosToken>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            lex: Logos::lexer(input),
        }
    }
}
