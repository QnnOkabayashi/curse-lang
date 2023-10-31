//! The core data types that show up in the AST.

mod def;
mod expr;
mod pat;
mod program;
mod record;
mod shared;
pub mod tok;
mod ty;

pub mod bikeshed {
    use crate::ast::tok;
    use curse_interner::Ident;
    use derive_more::From;

    #[derive(Debug, Clone, From)]
    pub struct DynamicImport {
        pub dynamic_import: tok::DynamicImport,
        pub module: Ident,
    }
}

pub use def::{
    ChoiceDef, ExplicitTypes, FunctionDef, GenericParams, StructDef, VariantDef, Variants,
};
pub use expr::{Appl, Arm, Closure, Expr, Param, Paren, Region, RegionKind, Symbol};
pub use pat::Pat;
pub use program::Program;
pub use record::Record;
pub use shared::{Constructor, Lit, Path};
pub use ty::{GenericArgs, NamedType, Type};
