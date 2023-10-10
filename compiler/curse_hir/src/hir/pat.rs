use crate::hir::{Constructor, Lit};
use bumpalo_thin_slice::ThinSlice;
use curse_interner::Ident;
use curse_span::{HasSpan, Span};
use std::fmt;

#[derive(Copy, Clone)]
pub struct Pat<'hir> {
    pub kind: PatKind<'hir>,
    pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub enum PatKind<'hir> {
    Lit(Lit),
    Record(ThinSlice<'hir, (Pat<'hir>, Option<Pat<'hir>>)>),
    Constructor(&'hir Constructor<'hir, Pat<'hir>>),
    Error,
}

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

impl From<Ident> for Pat<'_> {
    fn from(value: Ident) -> Self {
        Pat {
            kind: PatKind::Lit(Lit::Ident(value.symbol)),
            span: value.span(),
        }
    }
}
