use curse_ast::ast::tok;
use curse_span::{HasSpan, Span};
use logos::Logos;
use std::fmt;
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Clone, Debug)]
enum Word {
    // e.g. "hello"
    Ident,
    // e.g. "Hello"
    TypeIdent,
    // e.g. 1_000_000
    Integer,
    InvalidIdent,
    InvalidInteger,
}

// TODO(quinn): this function can definitely be improved
fn lex_word(lex: &logos::Lexer<'_, LogosToken>) -> Word {
    let mut chars = lex.slice().chars();
    let first = chars.next().expect("at least 1 because of `\\w+`");

    if first.is_ascii_digit() {
        if chars.all(|ch| matches!(ch, '0'..='9' | '_')) {
            Word::Integer
        } else {
            Word::InvalidInteger
        }
    } else if (is_xid_start(first) || first == '_') && chars.all(is_xid_continue) {
        if first.is_uppercase() {
            Word::TypeIdent
        } else {
            Word::Ident
        }
    } else {
        Word::InvalidIdent
    }
}

macro_rules! declare_tokens {
    ($($(#[$attr:meta])* $tok:literal => $name:ident,)*) => {

        #[derive(Clone, Debug, Logos)]
        #[logos(skip r"\s+")] // Whitespace
        #[logos(skip r"//[^\r\n]*")] // Comments
        enum LogosToken {
            #[regex("\\w+", lex_word)]
            Word(Word),
            // TODO(quinn): support escape sequences
            #[regex(r#""[^"]*""#)]
            StringLiteral,
            $(
                #[token($tok)]
                $name,
            )*
        }

        #[derive(Copy, Clone, Debug)]
        pub enum Token<'ast> {
            Ident(tok::Literal<'ast>),
            TypeIdent(tok::Literal<'ast>),
            Integer(tok::Literal<'ast>),
            // includes quotes on each end
            StringLiteral(tok::Literal<'ast>),
            $(
                $(#[$attr])*
                $name(tok::$name),
            )*
        }

        impl Token<'_> {
            pub fn span(&self) -> Span {
                match self {
                    Token::Ident(tok) => tok.span(),
                    Token::TypeIdent(tok) => tok.span(),
                    Token::Integer(tok) => tok.span(),
                    Token::StringLiteral(tok) => tok.span(),
                    $(
                        Token::$name(tok) => tok.span(),
                    )*
                }
            }
        }

        impl fmt::Display for Token<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Token::Ident(tok) => fmt::Display::fmt(tok, f),
                    Token::TypeIdent(tok) => fmt::Display::fmt(tok, f),
                    Token::Integer(tok) => fmt::Display::fmt(tok, f),
                    Token::StringLiteral(tok) => fmt::Display::fmt(tok, f),
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
                let span = Span { start: start as u32, end: end as u32 };
                let token = match token {
                    Ok(LogosToken::Word(word)) => match word {
                        Word::Ident => Token::Ident(tok::Literal {
                            location: span.start,
                            literal: self.lex.slice(),
                        }),
                        Word::TypeIdent => Token::TypeIdent(tok::Literal {
                            location: span.start,
                            literal: self.lex.slice(),
                        }),
                        Word::Integer => Token::Integer(tok::Literal {
                            location: span.start,
                            literal: self.lex.slice(),
                        }),
                        Word::InvalidIdent => return Some(Err(LexError::InvalidIdent(span))),
                        Word::InvalidInteger => return Some(Err(LexError::InvalidInteger(span))),
                    },
                    Ok(LogosToken::StringLiteral) => Token::StringLiteral(tok::Literal {
                        location: span.start,
                        literal: self.lex.slice(),
                    }),
                    $(
                        Ok(LogosToken::$name) => Token::$name(tok::$name {
                            location: span.start
                        }),
                    )*
                    Err(()) => return Some(Err(LexError::UnknownSeq(span))),
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
    "::" => ColonColon,
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
    "choice" => Choice,
    "struct" => Struct,
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
    "ref" => Ref,
    "mut" => Mut,
    "dynamic_import" => DynamicImport,
}

#[derive(Copy, Clone, Debug)]
pub enum LexError {
    UnknownSeq(Span),
    InvalidIdent(Span),
    InvalidInteger(Span),
}
