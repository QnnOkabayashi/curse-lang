use curse_ast::{tok, Generics, Nonempty, Type};
use typed_arena::Arena;

#[derive(Debug, Default)]
pub struct TyArena<'ast, 'input> {
    pub arena: Arena<Type<'ast, 'input>>,
}

impl<'ast, 'input> TyArena<'ast, 'input> {
}

