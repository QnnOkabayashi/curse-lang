use crate::Map;
use curse_interner::Ident;
use curse_span::{HasSpan, Span};
use std::{fmt, str::FromStr};

pub type TypeRef<'hir> = &'hir Type<'hir>;

pub struct Type<'hir> {
    pub kind: TypeKind<'hir>,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeKind<'hir> {
    Named(Ident, &'hir [Type<'hir>]),
    Generic(Ident, u32),
    Record(Map<'hir, Ident, TypeRef<'hir>>),
    Primitive(PrimitiveType),
    Error,
}

#[derive(Copy, Clone, Debug)]
pub enum PrimitiveType {
    I32,
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
