use crate::Span;
use std::fmt;

#[derive(Copy, Clone)]
pub struct Ident<'input> {
    pub location: usize,
    pub literal: &'input str,
}

impl Span for Ident<'_> {
    fn span(&self) -> (usize, usize) {
        (self.location, self.literal.len())
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.literal, f)
    }
}

impl fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.literal, f)
    }
}

#[derive(Copy, Clone)]
pub struct Integer<'input> {
    pub location: usize,
    pub literal: &'input str,
}

impl Span for Integer<'_> {
    fn span(&self) -> (usize, usize) {
        (self.location, self.literal.len())
    }
}

impl fmt::Display for Integer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.literal, f)
    }
}

impl fmt::Debug for Integer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.literal, f)
    }
}

/// NamedTypes are like idents, but start with a tick (`'`).
#[derive(Copy, Clone)]
pub struct NamedType<'input> {
    /// Index of the apostrophe
    pub location: usize,
    /// Spanning from the apostrophe to the end of the ident
    pub literal: &'input str,
}

impl Span for NamedType<'_> {
    fn span(&self) -> (usize, usize) {
        (self.location, self.literal.len())
    }
}

impl fmt::Display for NamedType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.literal, f)
    }
}

impl fmt::Debug for NamedType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.literal, f)
    }
}

macro_rules! declare_tokens {
    ($($(#[$attr:meta])* $tok:literal => $name:ident,)*) => {
        $(
            $(#[$attr])*
            #[derive(Copy, Clone, Default)]
            pub struct $name {
                pub location: usize,
            }

            impl Span for $name {
                fn span(&self) -> (usize, usize) {
                    (self.location, $tok.len())
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str($tok)
                }
            }

            impl fmt::Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "`{}`", $tok)
                }
            }
        )*

    }
}

// Any changes here must also be reflected in:
// - curse_parse/src/lexer.rs
// - curse_parse/src/grammar.lalrpop
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
    "else" => Else,
    "struct" => Struct,
    "choice" => Choice,
    "{" => LBrace,
    "}" => RBrace,
    "->" => Arrow,
    "'" => Apostrophe,

    "=" => Eq,
    "<" => Lt,
    ">" => Gt,
    "<=" => Le,
    ">=" => Ge,

    "true" => True,
    "false" => False,
}
