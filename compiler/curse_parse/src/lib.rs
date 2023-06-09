use curse_ast::{Expr, Pat, Type, Program};
use lalrpop_util::lalrpop_mod;

mod error;
pub use error::{Error, SourceErrors};

mod lexer;
use lexer::Lexer;

mod parse;

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar
);

#[derive(Default)]
pub struct Arena<'ast, 'input> {
    pub exprs: typed_arena::Arena<Expr<'ast, 'input>>,
    pub pats: typed_arena::Arena<Pat<'ast, 'input>>,
    pub types: typed_arena::Arena<Type<'ast, 'input>>,
}

pub trait ArenaAlloc<'ast, 'input>: Sized {
    fn alloc_in(self, arena: &'ast Arena<'ast, 'input>) -> &'ast Self;
}

impl<'ast, 'input> ArenaAlloc<'ast, 'input> for Expr<'ast, 'input> {
    fn alloc_in(self, arena: &'ast Arena<'ast, 'input>) -> &'ast Self {
        arena.exprs.alloc(self)
    }
}

impl<'ast, 'input> ArenaAlloc<'ast, 'input> for Pat<'ast, 'input> {
    fn alloc_in(self, arena: &'ast Arena<'ast, 'input>) -> &'ast Self {
        arena.pats.alloc(self)
    }
}

impl<'ast, 'input> ArenaAlloc<'ast, 'input> for Type<'ast, 'input> {
    fn alloc_in(self, arena: &'ast Arena<'ast, 'input>) -> &'ast Self {
        arena.types.alloc(self)
    }
}

impl<'ast, 'input> Arena<'ast, 'input> {
    fn alloc<T: ArenaAlloc<'ast, 'input>>(&'ast self, val: T) -> &'ast T {
        val.alloc_in(self)
    }

    pub(crate) fn alloc_expr(
        &'ast self,
    ) -> impl Fn(Expr<'ast, 'input>) -> &'ast Expr<'ast, 'input> {
        move |expr| self.exprs.alloc(expr)
    }

    pub(crate) fn alloc_pat(&'ast self) -> impl Fn(Pat<'ast, 'input>) -> &'ast Pat<'ast, 'input> {
        move |pat| self.pats.alloc(pat)
    }

    pub(crate) fn alloc_ty(&'ast self) -> impl Fn(Type<'ast, 'input>) -> &'ast Type<'ast, 'input> {
        move |ty| self.types.alloc(ty)
    }
}

pub fn parse_program<'ast, 'input>(
    ast: &'ast Arena<'ast, 'input>,
    input: &'input str,
) -> Result<Program<'ast, 'input>, Vec<Error>> {
    match grammar::ProgramParser::new().parse(ast, Lexer::new(input)) {
        Ok(program) => Ok(program),
        Err(err) => todo!(),
    }
}

pub fn parse_expr<'ast, 'input>(
    context: &'ast Arena<'ast, 'input>,
    input: &'input str,
) -> &'ast Expr<'ast, 'input> {
    match grammar::EndExprParser::new().parse(context, Lexer::new(input)) {
        Ok(_) => todo!(),
        Err(err) => todo!(),
    }
    // context.exprs.alloc()
}
