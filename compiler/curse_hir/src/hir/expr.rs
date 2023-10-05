use crate::hir::{Constructor, Binding, Lit, Pat, Type};
use bumpalo_thin_slice::ThinSlice;
use curse_interner::Ident;
use curse_span::{HasSpan, Span};
use std::fmt;

#[derive(Copy, Clone)]
pub struct Expr<'hir> {
    pub kind: ExprKind<'hir>,
    pub span: Span,
}

impl fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.kind, f)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ExprKind<'hir> {
    Symbol(Symbol),
    Lit(Lit),
    Record(ThinSlice<'hir, (Binding<'hir>, Option<Expr<'hir>>)>),
    Constructor(&'hir Constructor<'hir, Expr<'hir>>),
    Closure(ThinSlice<'hir, Arm<'hir>>),
    Appl(&'hir Appl<'hir>),
    Region(&'hir Region<'hir>),
    Error,
}

#[derive(Copy, Clone, Debug)]
pub enum Symbol {
    Plus,
    Minus,
    Star,
    Dot,
    DotDot,
    Semi,
    Percent,
    Slash,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Copy, Clone, Debug)]
pub struct Arm<'hir> {
    pub params: ThinSlice<'hir, Param<'hir>>,
    pub body: Expr<'hir>,
}

#[derive(Copy, Clone, Debug)]
pub struct Param<'hir> {
    pub pat: Pat<'hir>,
    pub ascription: Option<Type<'hir>>,
}

impl HasSpan for Param<'_> {
    fn start(&self) -> u32 {
        self.pat.start()
    }

    fn end(&self) -> u32 {
        if let Some(ty) = self.ascription {
            ty.end()
        } else {
            self.pat.end()
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Appl<'hir> {
    pub lhs: Expr<'hir>,
    pub fun: Expr<'hir>,
    pub rhs: Expr<'hir>,
}

#[derive(Copy, Clone, Debug)]
pub struct Region<'hir> {
    pub kind: RegionKind,
    // Don't want it to store an ident,
    // want it to store some semantic reference to the variable.
    pub shadows: &'hir [Ident],
    pub body: Expr<'hir>,
}

#[derive(Copy, Clone, Debug)]
pub enum RegionKind {
    Ref,
    Mut,
    RefMut,
}
