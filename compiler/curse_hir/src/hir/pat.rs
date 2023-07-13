use crate::hir::{Lit, Map};
use curse_interner::Ident;
use curse_span::{HasSpan, Span};
use std::fmt;

pub struct Pat<'hir> {
    pub kind: PatKind<'hir>,
    pub span: Span,
}

#[derive(Debug)]
pub enum PatKind<'hir> {
    Lit(Lit),
    Record(Map<'hir, Option<PatRef<'hir>>>),
    Constructor(&'hir [Ident], PatRef<'hir>),
    Error,
}

pub type PatRef<'hir> = &'hir Pat<'hir>;

impl HasSpan for Pat<'_> {
    fn start(&self) -> u32 {
        self.span.start
    }

    fn end(&self) -> u32 {
        self.span.end
    }
}

impl fmt::Debug for Pat<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.kind, f)
    }
}
