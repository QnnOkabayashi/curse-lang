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
    use curse_interner::Ident;
    use crate::ast::tok;

    crate::ast_struct! {
        #[derive(Debug, Clone)]
        pub struct DynamicImport {
            pub dynamic_import: tok::DynamicImport,
            pub module: Ident,
        }
    }
}

pub use def::{
    ChoiceDef, ExplicitTypes, FunctionDef, GenericParams, StructDef, VariantDef, Variants,
};
pub use expr::{Appl, Arm, Closure, Expr, Param, Paren, Region, RegionKind, Symbol};
pub use pat::Pat;
pub use program::Program;
pub use record::{Field, Record};
pub use shared::{Constructor, Iter, Lit, Path};
pub use ty::{GenericArgs, NamedType, Type};

/// Macro to automatically derive a `new` constructor.
#[macro_export]
macro_rules! ast_struct {
(
    $(#[$attrs:meta])*
    $vis:vis struct $name:ident <$($generics:tt),*> {
        $($field_vis:vis $field:ident : $ty:ty,)*
    }
) => {
        $(#[$attrs])*
        $vis struct $name <$($generics),*> {
            $($field_vis $field : $ty,)*
        }

        impl<$($generics),*> $name <$($generics),*> {
            pub fn new($($field : $ty),*) -> $name <$($generics),*> {
                $name { $($field),* }
            }
        }
    };


(
    $(#[$attrs:meta])*
    $vis:vis struct $name:ident {
        $($field_vis:vis $field:ident : $ty:ty,)*
    }
) => {
        $(#[$attrs])*
        $vis struct $name {
            $($field_vis $field : $ty,)*
        }

        impl $name {
            pub fn new($($field : $ty),*) -> $name {
                $name { $($field),* }
            }
        }
    };
}
