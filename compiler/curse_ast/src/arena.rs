//! The arena storage used to manage AST memory.

use crate::ast::{Closure, Expr, Pat, Type};
use typed_arena::Arena;

#[derive(Default)]
pub struct AstArena<'ast, 'input> {
    pub exprs: Arena<Expr<'ast, 'input>>,
    pub closures: Arena<Closure<'ast, 'input>>,
    pub pats: Arena<Pat<'ast, 'input>>,
    pub types: Arena<Type<'ast, 'input>>,
}
