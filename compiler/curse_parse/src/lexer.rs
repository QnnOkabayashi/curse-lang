use curse_ast::{tok, Span};
use logos::Logos;
use std::fmt;

macro_rules! declare_tokens {
    ($($(#[$attr:meta])* $tok:literal => $name:ident,)*) => {

        #[derive(Clone, Debug, Logos)]
        #[logos(skip r"[ \t\v\r\n\f]+")] // Whitespace
        #[logos(skip r"//[^\r\n]*")] // Comments
        enum LogosToken {
            /// TODO(quinn): Add support for all unicode identifiers
            #[regex("[_a-z][_a-zA-Z0-9]*")]
            Ident,
            #[regex("[_]*[A-Z][_a-zA-Z0-9]*", priority = 2)]
            NamedType,
            #[regex("[0-9]+")]
            Integer,
            $(
                #[token($tok)]
                $name,
            )*
        }

        #[derive(Copy, Clone, Debug)]
        pub enum Token<'input> {
            Ident(tok::Ident<'input>),
            Integer(tok::Integer<'input>),
            NamedType(tok::NamedType<'input>),
            $(
                $(#[$attr])*
                $name(tok::$name),
            )*
        }

        impl Token<'_> {
            pub fn span(&self) -> (usize, usize) {
                match self {
                    Token::Ident(tok) => tok.span(),
                    Token::Integer(tok) => tok.span(),
                    Token::NamedType(tok) => tok.span(),
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
                    Token::NamedType(tok) => f.write_str(tok.literal),
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
                let std::ops::Range { start, end } = self.lex.span();
                let token = match token {
                    Ok(LogosToken::Ident) => Token::Ident(tok::Ident {
                        location: start,
                        literal: self.lex.slice(),
                    }),
                    Ok(LogosToken::Integer) => Token::Integer(tok::Integer {
                        location: start,
                        literal: self.lex.slice(),
                    }),
                    Ok(LogosToken::NamedType) => Token::NamedType(tok::NamedType {
                        location: start,
                        literal: self.lex.slice(),
                    }),
                    $(
                        Ok(LogosToken::$name) => Token::$name(tok::$name {
                            location: start,
                        }),
                    )*
                    Err(()) => return Some(Err(LexError {
                        span: (start, end - start),
                    })),
                };

                Some(Ok((start, token, end)))
            }
        }
    }
}

// Any changes here must also be reflected in:
// - curse_parse/src/grammar.lalrpop
// - curse_ast/src/tok.rs
declare_tokens! {
    ":" => Colon,
    "," => Comma,
    "(" => LParen,
    ")" => RParen,
    "+" => Plus,
    "-" => Minus,
    "*" => Star,
    "." => Dot,
    ".." => DotDot,
    ";" => Semi,
    "%" => Percent,
    "/" => Slash,
    "|" => Pipe,
    "fn" => Fn,
    "struct" => Struct,
    "choice" => Choice,
    "{" => LBrace,
    "}" => RBrace,
    "->" => Arrow,

    "=" => Eq,
    "<" => Lt,
    ">" => Gt,
    "<=" => Le,
    ">=" => Ge,

    "true" => True,
    "false" => False,
}

#[derive(Copy, Clone, Debug)]
pub struct LexError {
    pub span: (usize, usize),
}
