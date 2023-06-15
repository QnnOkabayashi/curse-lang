//! Lowers the AST to the HIR.

use curse_arena::DroplessArena;
use curse_hir::{Arm, Expr, ExprRef, Param, Pat, PatRef, Type, TypeRef};
use curse_interner::Ident;

mod error;
mod lowerer;

pub use error::{LoweringError, UnexpectedTypeArgs};
pub use lowerer::Lowerer;

#[derive(Default)]
pub struct Arena<'hir> {
    // defs
    pub idents: DroplessArena<Ident>,
    pub variant_defs: DroplessArena<(Ident, TypeRef<'hir>)>,

    // exprs
    pub exprs: DroplessArena<Expr<'hir>>,
    pub expr_fields: DroplessArena<(Ident, Option<ExprRef<'hir>>)>,
    pub arms: DroplessArena<Arm<'hir>>,
    pub params: DroplessArena<Param<'hir>>,

    // pats
    pub pats: DroplessArena<Pat<'hir>>,
    pub pat_fields: DroplessArena<(Ident, Option<PatRef<'hir>>)>,

    // types
    pub types: DroplessArena<Type<'hir>>,
    pub type_fields: DroplessArena<(Ident, TypeRef<'hir>)>,
}
