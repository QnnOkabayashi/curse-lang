use crate::hir::Pat;
use bumpalo_thin_slice::ThinSlice;
use curse_interner::Ident;
use curse_span::{HasSpan, Span};
use std::{fmt, str::FromStr};

#[derive(Copy, Clone)]
pub struct Type<'hir> {
    pub kind: TypeKind<'hir>,
    pub span: Span,
}

impl<'hir> Type<'hir> {
    pub fn error(span: Span) -> Self {
        Type {
            kind: TypeKind::Error,
            span,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TypeKind<'hir> {
    /// The path and the type arguments, e.g. `std::result::Result (I32 * Error)`
    Named {
        path: ThinSlice<'hir, Ident>,
        generic_args: ThinSlice<'hir, Type<'hir>>,
    },
    /// A generic type argument and the index, e.g. `T`
    Generic {
        name: Ident,
        index: u32,
    },
    /// A record type, e.g. `{ key: K, value: V }`
    Record(ThinSlice<'hir, (Pat<'hir>, Type<'hir>)>),
    /// A primitive type, e.g. `I32`
    Primitive(PrimitiveType),
    Error,
}

#[derive(Copy, Clone, Debug)]
pub enum PrimitiveType {
    /// 32-bit signed integer.
    I32,
    /// Boolean.
    Bool,
}

impl HasSpan for Type<'_> {
    fn start(&self) -> u32 {
        self.span.start
    }

    fn end(&self) -> u32 {
        self.span.end
    }
}

impl fmt::Debug for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.kind, f)
    }
}

impl FromStr for PrimitiveType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "I32" => Ok(Self::I32),
            "Bool" => Ok(Self::Bool),
            _ => Err(()),
        }
    }
}
