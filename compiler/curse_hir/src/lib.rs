//! The Curse High-level Intermediate Representation (HIR).
//!
//! Differences from AST:
//! 1. All identifiers are interned.
//! 2. Tokens used for disambiguation are removed (e.g. parenthesized exprs, commas, ...).
//! 3. All sequences are allocated as contiguous slices in an arena.
//!
//! Essentially, the HIR is a boiled down version of the AST, in the sense that it cuts away
//! everything that's unnecessary for analysis, but doesn't actually do any analysis yet.

mod def;
mod expr;
mod map;
mod pat;
mod ty;
mod program {
    use crate::{ChoiceDef, FunctionDef, StructDef};
    use curse_interner::InternedString;
    use std::collections::HashMap;

    #[derive(Debug)]
    pub struct Program<'hir> {
        pub function_defs: HashMap<InternedString, FunctionDef<'hir>>,
        pub struct_defs: HashMap<InternedString, StructDef<'hir>>,
        pub choice_defs: HashMap<InternedString, ChoiceDef<'hir>>,
    }
}

pub use def::{ChoiceDef, FunctionDef, StructDef};
pub use expr::{Appl, Arm, Expr, ExprKind, ExprRef, Lit, Param, Symbol};
pub use map::Map;
pub use pat::{Pat, PatKind, PatRef};
pub use program::Program;
pub use ty::{PrimitiveType, Type, TypeKind, TypeRef};
