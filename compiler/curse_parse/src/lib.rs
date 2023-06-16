use curse_ast::{Expr, Pat, Program, Type};
use lalrpop_util::lalrpop_mod;
use typed_arena as ta;

mod error;
pub use error::Error;

mod lexer;
use lexer::Lexer;

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar
);

pub struct Parser<'ast, 'input> {
    pub arena: &'ast Arena<'ast, 'input>,
    pub errors: Vec<Error>,
}

impl<'ast, 'input> Parser<'ast, 'input> {
    pub fn new(arena: &'ast Arena<'ast, 'input>) -> Self {
        Parser {
            arena,
            errors: Vec::with_capacity(0),
        }
    }

    pub fn parse_program(&mut self, input: &'input str) -> Program<'ast, 'input> {
        grammar::ProgramParser::new()
            .parse(self, Lexer::new(input))
            .expect("`Program` rule recovers from all errors")
    }

    pub fn parse_expr(&mut self, input: &'input str) -> Expr<'ast, 'input> {
        grammar::EndExprParser::new()
            .parse(self, Lexer::new(input))
            .expect("`EndExpr` rule recovers from all errors")
    }
}

// Cannot use `curse_arena::DroplessArena` because these types own allocations.
#[derive(Default)]
pub struct Arena<'ast, 'input> {
    pub exprs: ta::Arena<Expr<'ast, 'input>>,
    pub pats: ta::Arena<Pat<'ast, 'input>>,
    pub types: ta::Arena<Type<'ast, 'input>>,
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
}
