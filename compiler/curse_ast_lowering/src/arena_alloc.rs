use curse_arena::new::Arena;
use curse_hir::arena::HirArena;
use curse_hir::hir::{Arm, Expr, ExprRef, Param, Pat, PatRef, Type, TypeRef};
use curse_interner::Ident;

pub trait ArenaAlloc<'hir>: Sized {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self>;
}

impl<'hir> ArenaAlloc<'hir> for Ident {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.idents
    }
}

impl<'hir> ArenaAlloc<'hir> for (Ident, TypeRef<'hir>) {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.ident_and_type
    }
}

impl<'hir> ArenaAlloc<'hir> for Expr<'hir> {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.exprs
    }
}

impl<'hir> ArenaAlloc<'hir> for (Ident, Option<ExprRef<'hir>>) {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.ident_and_opt_expr
    }
}

impl<'hir> ArenaAlloc<'hir> for Arm<'hir> {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.arms
    }
}

impl<'hir> ArenaAlloc<'hir> for Param<'hir> {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.params
    }
}

impl<'hir> ArenaAlloc<'hir> for Pat<'hir> {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.pats
    }
}

impl<'hir> ArenaAlloc<'hir> for (Ident, Option<PatRef<'hir>>) {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.ident_and_opt_pat
    }
}

impl<'hir> ArenaAlloc<'hir> for Type<'hir> {
    fn get_arena(arena: &'hir HirArena<'hir>) -> &'hir Arena<Self> {
        &arena.types
    }
}

