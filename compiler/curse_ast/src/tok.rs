use displaydoc::Display;
use std::fmt;

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("{literal}")]
pub struct Ident<'input> {
    pub location: usize,
    pub literal: &'input str,
}

impl Ident<'_> {
    pub fn span(&self) -> (usize, usize) {
        (self.location, self.location + self.literal.len())
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("{literal}")]
pub struct Integer<'input> {
    pub location: usize,
    pub literal: &'input str,
}

impl Integer<'_> {
    pub fn span(&self) -> (usize, usize) {
        (self.location, self.location + self.literal.len())
    }
}

macro_rules! declare_tokens {
    ($($(#[$attr:meta])* $tok:literal => $name:ident,)*) => {
        $(
            $(#[$attr])*
            #[derive(Copy, Clone, Debug)]
            pub struct $name {
                pub location: usize,
            }

            impl $name {
                pub fn span(&self) -> (usize, usize) {
                    (self.location, self.location + $tok.len())
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str($tok)
                }
            }
        )*

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
