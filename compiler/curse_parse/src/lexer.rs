use curse_ast::tok;
use logos::Logos;
use std::{fmt, ops::Range};

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
            pub fn span(&self) -> Range<usize> {
                match self {
                    Token::Ident(tok) => tok.span(),
                    Token::Integer(tok) => tok.span(),
                    $(
                        Token::$name(tok) => tok.span(),
                    )*
                }
            }
        }

        impl fmt::Display for Token<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Token::Ident(tok) => f.write_str(tok.literal),
                    Token::Integer(tok) => f.write_str(tok.literal),
                    $(
                        Token::$name(_) => f.write_str($tok),
                    )*
                }
            }
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

        impl<'input> Iterator for Lexer<'input> {
            type Item = Result<(usize, Token<'input>, usize), LexError>;

            fn next(&mut self) -> Option<Self::Item> {
                let token = self.lex.next()?;
                let Range { start, end } = self.lex.span();
                let token = match token {
                    LogosToken::Ident => Token::Ident(tok::Ident {
                        location: start,
                        literal: self.lex.slice(),
                    }),
                    LogosToken::Integer => Token::Integer(tok::Integer {
                        location: start,
                        literal: self.lex.slice(),
                    }),
                    $(
                        LogosToken::$name => Token::$name(tok::$name {
                            location: start,
                        }),
                    )*
                    LogosToken::Unknown => return Some(Err(LexError {
                        span: (start, end),
                    })),
                    _ => unreachable!("remaining patterns are skipped"),
                };

                Some(Ok((start, token, end)))
            }
        }
    }
}

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
    "struct" => Struct,
    "enum" => Enum,
    "{" => LBrace,
    "}" => RBrace,
    "->" => Arrow,

    "=" => Equal,
    "<" => Less,
    ">" => Greater,
    "<=" => LessEqual,
    ">=" => GreaterEqual,

    "true" => True,
    "false" => False,
}

#[derive(Copy, Clone, Debug)]
pub struct LexError {
    pub span: (usize, usize),
}
