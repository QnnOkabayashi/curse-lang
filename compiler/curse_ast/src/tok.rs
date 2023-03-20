use std::fmt;

#[derive(Copy, Clone, Debug)]
pub struct Ident<'input> {
    pub location: usize,
    pub literal: &'input str,
}

impl Ident<'_> {
    pub fn span(&self) -> (usize, usize) {
        (self.location, self.literal.len())
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.literal)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Integer<'input> {
    pub location: usize,
    pub literal: &'input str,
}

impl Integer<'_> {
    pub fn span(&self) -> (usize, usize) {
        (self.location, self.literal.len())
    }
}

impl fmt::Display for Integer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.literal)
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
                    (self.location, $tok.len())
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
    "." => Dot,
    ".." => DotDot,
    ";" => Semi,
    "%" => Percent,
    "/" => Slash,
    "|" => Pipe,
    "let" => Let,
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
