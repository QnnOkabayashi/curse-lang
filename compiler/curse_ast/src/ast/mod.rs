//! The core data types that show up in the AST.

mod def;
mod expr;
mod pat;
mod program;
mod record;
mod shared;
pub mod tok;
mod ty;

pub use def::{
    ChoiceDef, ExplicitTypes, FunctionDef, GenericParams, StructDef, VariantDef, Variants,
};
pub use expr::{Appl, Arm, Closure, Expr, ExprRef, Param, Paren, Symbol};
pub use pat::{Pat, PatRef};
pub use program::Program;
pub use record::{Field, Record};
pub use shared::{Constructor, Lit, Path};
pub use ty::{GenericArgs, NamedType, Type, TypeRef};

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
}
