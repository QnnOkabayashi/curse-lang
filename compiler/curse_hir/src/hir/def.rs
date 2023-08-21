use crate::hir::{Arm, Map, TypeRef};
use curse_interner::Ident;
use curse_span::{HasSpan, Span};

#[derive(Debug)]
pub struct FunctionDef<'hir> {
    pub ident: Ident,
    pub generic_params: &'hir [Ident],
    pub ty: Option<TypeRef<'hir>>,
    pub arms: &'hir [Arm<'hir>],
    pub span: Span,
}

#[derive(Debug)]
pub struct StructDef<'hir> {
    pub ident: Ident,
    pub generic_params: &'hir [Ident],
    pub ty: TypeRef<'hir>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ChoiceDef<'hir> {
    pub ident: Ident,
    pub generic_params: &'hir [Ident],
    pub variants: Map<'hir, TypeRef<'hir>>,
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
