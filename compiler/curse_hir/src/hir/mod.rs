mod def;
mod expr;
mod map;
mod pat;
mod ty;
mod program {
    use crate::hir::{ChoiceDef, FunctionDef, StructDef};
    use curse_interner::{Ident, InternedString};
    use std::collections::{HashMap, HashSet};

    #[derive(Debug)]
    pub struct Program<'hir> {
        pub function_defs: HashMap<InternedString, FunctionDef<'hir>>,
        pub struct_defs: HashMap<InternedString, StructDef<'hir>>,
        pub choice_defs: HashMap<InternedString, ChoiceDef<'hir>>,
        pub dynamic_imports: HashSet<Ident>,
    }
}
mod shared {
    use curse_interner::{Ident, InternedString};

    #[derive(Copy, Clone, Debug)]
    pub enum Lit {
        Integer(u32),
        Ident(InternedString),
        Bool(bool),
    }

    #[derive(Copy, Clone, Debug)]
    pub struct Constructor<'hir, Kind> {
        pub ty: Ident,
        pub variant: Ident,
        pub kind: &'hir Kind,
    }
}

pub use def::{ChoiceDef, FunctionDef, StructDef};
pub use expr::{Appl, Arm, Expr, ExprKind, Param, Region, RegionKind, Symbol};
pub use map::Binding;
pub use pat::{Pat, PatKind};
pub use program::Program;
pub use shared::{Constructor, Lit};
pub use ty::{PrimitiveType, Type, TypeKind};
