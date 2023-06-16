use crate::{Map, PatRef, TypeRef};
use curse_interner::Ident;
use curse_span::{HasSpan, Span};
use std::fmt;

#[derive(Clone)]
pub struct Expr<'hir> {
    pub kind: ExprKind<'hir>,
    pub span: Span,
}

pub type ExprRef<'hir> = &'hir Expr<'hir>;

impl fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.kind, f)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ExprKind<'hir> {
    Symbol(Symbol),
    Lit(Lit),
    Record(Map<'hir, Ident, Option<ExprRef<'hir>>>),
    Constructor(Ident, ExprRef<'hir>),
    Closure(&'hir [Arm<'hir>]),
    Appl(Appl<'hir>),
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
pub enum Lit {
    Integer(u32),
    Ident(Ident),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub struct Arm<'hir> {
    pub params: &'hir [Param<'hir>],
    pub body: ExprRef<'hir>,
}

#[derive(Copy, Clone, Debug)]
pub struct Param<'hir> {
    pub pat: PatRef<'hir>,
    pub ascription: Option<TypeRef<'hir>>,
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

#[derive(Copy, Clone)]
pub struct Appl<'hir> {
    pub parts: &'hir [Expr<'hir>; 3],
}

impl<'hir> Appl<'hir> {
    pub fn lhs(&self) -> &'hir Expr<'hir> {
        &self.parts[0]
    }

    pub fn fun(&self) -> &'hir Expr<'hir> {
        &self.parts[1]
    }

    pub fn rhs(&self) -> &'hir Expr<'hir> {
        &self.parts[2]
    }
}

impl fmt::Debug for Appl<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Appl")
            .field("lhs", self.lhs())
            .field("fun", self.fun())
            .field("rhs", self.rhs())
            .finish()
    }
}
