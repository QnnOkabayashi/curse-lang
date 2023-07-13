use curse_span::HasSpan;
use std::fmt;

#[derive(Copy, Clone)]
pub struct Ident<'input> {
    pub location: u32,
    pub literal: &'input str,
}

impl AsRef<str> for Ident<'_> {
    fn as_ref(&self) -> &str {
        self.literal
    }
}

impl HasSpan for Ident<'_> {
    fn start(&self) -> u32 {
        self.location
    }

    fn end(&self) -> u32 {
        self.location + self.literal.len() as u32
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
    pub location: u32,
    pub literal: &'input str,
}

impl HasSpan for Integer<'_> {
    fn start(&self) -> u32 {
        self.location
    }

    fn end(&self) -> u32 {
        self.location + self.literal.len() as u32
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

macro_rules! declare_tokens {
    ($($(#[$attr:meta])* $tok:literal => $name:ident,)*) => {
        $(
            $(#[$attr])*
            #[derive(Copy, Clone, Default)]
            pub struct $name {
                pub location: u32,
            }

            impl HasSpan for $name {
                fn start(&self) -> u32 {
                    self.location
                }

                fn end(&self) -> u32 {
                    self.location + $tok.len() as u32
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
}
