use curse_ast::expr::Lit;
use curse_ast::pat::{Nonempty, Pat};
use typed_arena::Arena;

#[derive(Debug, Default)]
pub struct PatArena<'ast, 'input> {
    arena: Arena<Pat<'ast, 'input>>,
}

impl<'ast, 'input> PatArena<'ast, 'input> {
}

