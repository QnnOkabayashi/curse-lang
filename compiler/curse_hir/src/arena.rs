use crate::hir::{Arm, Expr, ExprRef, Param, Pat, PatRef, Type, TypeRef};
use curse_arena::new::Arena;
use curse_interner::Ident;

pub struct HirArena<'hir> {
    /// Generic parameters, e.g. `(A * B * C)`,
    /// and paths, e.g. `std::alloc::Vec`.
    pub idents: Arena<Ident>,

    /// Record types, e.g. `{ online: Bool, age: I32 }`,
    /// and choice variants, e.g. `Some T | None {}`.
    pub ident_and_type: Arena<(Ident, TypeRef<'hir>)>,
    pub exprs: Arena<Expr<'hir>>,

    /// Record expressions, e.g. `{ online: true, age }`.
    pub ident_and_opt_expr: Arena<(Ident, Option<ExprRef<'hir>>)>,
    pub arms: Arena<Arm<'hir>>,
    pub params: Arena<Param<'hir>>,
    pub pats: Arena<Pat<'hir>>,

    /// Record patterns, e.g. `{ online, age }`.
    pub ident_and_opt_pat: Arena<(Ident, Option<PatRef<'hir>>)>,
    pub types: Arena<Type<'hir>>,
}


/// A counter that informs the HIR exactly how many elements to allocate for.
#[derive(Default)]
struct AllocationCounter {
    pub idents: usize,
    pub ident_and_type: usize,
    pub exprs: usize,
    pub ident_and_opt_expr: usize,
    pub arms: usize,
    pub params: usize,
    pub pats: usize,
    pub ident_and_opt_pat: usize,
    pub types: usize,
}

impl From<&AllocationCounter> for HirArena<'_> {
    fn from(counter: &AllocationCounter) -> Self {
        HirArena {
            idents: Arena::with_capacity(counter.idents),
            ident_and_type: Arena::with_capacity(counter.ident_and_type),
            exprs: Arena::with_capacity(counter.exprs),
            ident_and_opt_expr: Arena::with_capacity(counter.ident_and_opt_expr),
            arms: Arena::with_capacity(counter.arms),
            params: Arena::with_capacity(counter.params),
            pats: Arena::with_capacity(counter.pats),
            ident_and_opt_pat: Arena::with_capacity(counter.ident_and_opt_pat),
            types: Arena::with_capacity(counter.types),
        }
    }
}
