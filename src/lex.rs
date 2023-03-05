use logos::Logos;
use std::ops::Range;

type Loc = usize;
type Span = Range<Loc>;

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
                $(#[$attr])*
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

            #[derive(Clone, Debug)]
            pub struct Ident<'input>(pub Span, pub &'input str);

            #[derive(Clone, Debug)]
            pub struct Integer<'input>(pub Span, pub &'input str);

            $(
                $(#[$attr])*
                #[derive(Clone, Debug)]
                pub struct $name(pub Span);
            )*
        }

        #[derive(Clone, Debug)]
        pub enum Token<'input> {
            Ident(tok::Ident<'input>),
            Integer(tok::Integer<'input>),
            $(
                $(#[$attr])*
                $name(tok::$name),
            )*
        }

        impl<'input> Iterator for Lexer<'input> {
            type Item = Result<(Loc, Token<'input>, Loc), LexError>;

            fn next(&mut self) -> Option<Self::Item> {
                let token = match self.lex.next()? {
                    LogosToken::Ident => Token::Ident(tok::Ident(self.lex.span(), self.lex.slice())),
                    LogosToken::Integer => Token::Integer(tok::Integer(self.lex.span(), self.lex.slice())),
                    $(
                        LogosToken::$name => Token::$name(tok::$name(self.lex.span())),
                    )*
                    LogosToken::Unknown => return Some(Err(LexError)),
                    _ => unreachable!("remaining patterns are skipped"),
                };

                let Range { start, end } = self.lex.span();
                Some(Ok((start, token, end)))
            }
        }
    }
}

declare_tokens! {
    "fn" => Fn,
    ":" => Colon,
    "," => Comma,
    "(" => LParen,
    ")" => RParen,
    "=" => Equal,
    "+" => Plus,
    "-" => Minus,
    "*" => Star,
    ".." => DotDot,
    ";" => Semi,
    "%" => Percent,
    "/" => Slash,
    "|" => Pipe,
    "else" => Else,
}

#[derive(Debug)]
pub struct LexError;

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
