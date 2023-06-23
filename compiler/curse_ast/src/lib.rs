mod def;
mod path {
    use crate::{ast_struct, tok};

    ast_struct! {
        pub struct Path<'input, T> {
            pub items: Vec<(tok::Ident<'input>, tok::Colon)>,
            pub value: T,
        }
    }
}
mod expr;
mod pat;
mod record;
pub mod tok;
mod ty;
mod program {
    use crate::{ChoiceDef, FunctionDef, StructDef};

    #[derive(Clone, Debug, Default)]
    pub struct Program<'ast, 'input> {
        pub function_defs: Vec<FunctionDef<'ast, 'input>>,
        pub struct_defs: Vec<StructDef<'ast, 'input>>,
        pub choice_defs: Vec<ChoiceDef<'ast, 'input>>,
    }

    impl<'ast, 'input> Program<'ast, 'input> {
        pub fn with_function_def(mut self, function_def: FunctionDef<'ast, 'input>) -> Self {
            self.function_defs.push(function_def);
            self
        }

        pub fn with_struct_def(mut self, struct_def: StructDef<'ast, 'input>) -> Self {
            self.struct_defs.push(struct_def);
            self
        }

        pub fn with_choice_def(mut self, choice_def: ChoiceDef<'ast, 'input>) -> Self {
            self.choice_defs.push(choice_def);
            self
        }
    }
}

pub use def::{
    ChoiceDef, ExplicitTypes, FunctionDef, GenericParams, StructDef, VariantDef, Variants,
};
pub use expr::{Appl, Arm, Closure, Constructor, Expr, Lit, Param, Paren, Symbol};
pub use pat::Pat;
pub use path::Path;
pub use program::Program;
pub use record::{Field, Record};
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
                $name {
                    $($field),*
                }
            }
        }
    };
}
