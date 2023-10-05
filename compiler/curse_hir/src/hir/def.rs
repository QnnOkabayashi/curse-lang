use crate::hir::{Arm, Type};
use bumpalo_thin_slice::ThinSlice;
use curse_interner::Ident;
use curse_span::{HasSpan, Span};

#[derive(Debug)]
pub struct FunctionDef<'hir> {
    pub ident: Ident,
    pub generic_params: ThinSlice<'hir, Ident>,
    pub ty: Option<Type<'hir>>,
    pub arms: ThinSlice<'hir, Arm<'hir>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct StructDef<'hir> {
    pub ident: Ident,
    pub generic_params: ThinSlice<'hir, Ident>,
    pub ty: Type<'hir>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ChoiceDef<'hir> {
    pub ident: Ident,
    pub generic_params: ThinSlice<'hir, Ident>,
    pub variants: ThinSlice<'hir, (Ident, Type<'hir>)>,
    pub span: Span,
}

impl HasSpan for FunctionDef<'_> {
    fn start(&self) -> u32 {
        self.span.start
    }

    fn end(&self) -> u32 {
        self.span.end
    }
}

impl HasSpan for StructDef<'_> {
    fn start(&self) -> u32 {
        self.span.start
    }

    fn end(&self) -> u32 {
        self.span.end
    }
}

impl HasSpan for ChoiceDef<'_> {
    fn start(&self) -> u32 {
        self.span.start
    }

    fn end(&self) -> u32 {
        self.span.end
    }
}
