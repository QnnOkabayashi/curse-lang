mod def;
mod expr;
mod map;
mod pat;
mod ty;
mod program {
    use crate::hir::{ChoiceDef, FunctionDef, StructDef};
    use curse_interner::InternedString;
    use std::collections::HashMap;

    #[derive(Debug)]
    pub struct Program<'hir> {
        pub function_defs: HashMap<InternedString, FunctionDef<'hir>>,
        pub struct_defs: HashMap<InternedString, StructDef<'hir>>,
        pub choice_defs: HashMap<InternedString, ChoiceDef<'hir>>,
    }
}
mod shared {
    use curse_interner::Ident;

    #[derive(Copy, Clone, Debug)]
    pub enum Lit {
        Integer(u32),
        Ident(Ident),
        Bool(bool),
    }

    #[derive(Copy, Clone, Debug)]
    pub struct Constructor<'hir, T> {
        pub path: Path<'hir>,
        pub inner: &'hir T,
    }

    pub type Path<'hir> = &'hir [Ident];
}

pub use def::{ChoiceDef, FunctionDef, StructDef};
pub use expr::{Appl, Arm, Expr, ExprKind, ExprRef, Param, Symbol};
pub use map::Map;
pub use pat::{Pat, PatKind, PatRef};
pub use program::Program;
pub use shared::{Constructor, Lit, Path};
pub use ty::{PrimitiveType, Type, TypeKind, TypeRef};

