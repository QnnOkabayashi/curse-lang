use logos::Logos;
use std::ops::Range;

type Loc = usize;
type Span = Range<Loc>;

#[derive(Clone, Debug, Logos)]
#[logos(subpattern ws = r"[ \t\v\r\n\f]")]
enum LogosToken {
    #[regex("[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident,
    #[regex("[0-9]+")]
    Integer,
    #[token("fn")]
    Fn,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("=")]
    Equal,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("..")]
    DotDot,
    #[token(";")]
    Semi,
    #[token("%")]
    Percent,
    #[token("/")]
    Slash,
    #[token("|")]
    Pipe,
    #[token("else")]
    Else,

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
    #[derive(Clone, Debug)]
    pub struct Fn(pub Span);
    #[derive(Clone, Debug)]
    pub struct Colon(pub Span);
    #[derive(Clone, Debug)]
    pub struct Comma(pub Span);
    #[derive(Clone, Debug)]
    pub struct LParen(pub Span);
    #[derive(Clone, Debug)]
    pub struct RParen(pub Span);
    #[derive(Clone, Debug)]
    pub struct Equal(pub Span);
    #[derive(Clone, Debug)]
    pub struct Plus(pub Span);
    #[derive(Clone, Debug)]
    pub struct Minus(pub Span);
    #[derive(Clone, Debug)]
    pub struct Star(pub Span);
    #[derive(Clone, Debug)]
    pub struct DotDot(pub Span);
    #[derive(Clone, Debug)]
    pub struct Semi(pub Span);
    #[derive(Clone, Debug)]
    pub struct Percent(pub Span);
    #[derive(Clone, Debug)]
    pub struct Slash(pub Span);
    #[derive(Clone, Debug)]
    pub struct Pipe(pub Span);
    #[derive(Clone, Debug)]
    pub struct Else(pub Span);
}

#[derive(Clone, Debug)]
pub enum Token<'input> {
    Ident(tok::Ident<'input>),
    Integer(tok::Integer<'input>),
    Fn(tok::Fn),
    Colon(tok::Colon),
    Comma(tok::Comma),
    LParen(tok::LParen),
    RParen(tok::RParen),
    Equal(tok::Equal),
    Plus(tok::Plus),
    Minus(tok::Minus),
    Star(tok::Star),
    DotDot(tok::DotDot),
    Semi(tok::Semi),
    Percent(tok::Percent),
    Slash(tok::Slash),
    Pipe(tok::Pipe),
    Else(tok::Else),
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

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(Loc, Token<'input>, Loc), LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;
        let token = match self.lex.next()? {
            LogosToken::Ident => Ident(tok::Ident(self.lex.span(), self.lex.slice())),
            LogosToken::Integer => Integer(tok::Integer(self.lex.span(), self.lex.slice())),
            LogosToken::Fn => Fn(tok::Fn(self.lex.span())),
            LogosToken::Colon => Colon(tok::Colon(self.lex.span())),
            LogosToken::Comma => Comma(tok::Comma(self.lex.span())),
            LogosToken::LParen => LParen(tok::LParen(self.lex.span())),
            LogosToken::RParen => RParen(tok::RParen(self.lex.span())),
            LogosToken::Equal => Equal(tok::Equal(self.lex.span())),
            LogosToken::Plus => Plus(tok::Plus(self.lex.span())),
            LogosToken::Minus => Minus(tok::Minus(self.lex.span())),
            LogosToken::Star => Star(tok::Star(self.lex.span())),
            LogosToken::DotDot => DotDot(tok::DotDot(self.lex.span())),
            LogosToken::Semi => Semi(tok::Semi(self.lex.span())),
            LogosToken::Percent => Percent(tok::Percent(self.lex.span())),
            LogosToken::Slash => Slash(tok::Slash(self.lex.span())),
            LogosToken::Pipe => Pipe(tok::Pipe(self.lex.span())),
            LogosToken::Else => Else(tok::Else(self.lex.span())),
            LogosToken::Unknown => return Some(Err(LexError)),
            _ => unreachable!("remaining patterns are skipped"),
        };

        let Range { start, end } = self.lex.span();
        Some(Ok((start, token, end)))
    }
}
