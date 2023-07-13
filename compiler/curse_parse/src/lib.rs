use curse_ast::arena::AstArena;
use curse_ast::ast;
use lalrpop_util::lalrpop_mod;

mod error;
pub use error::Error;

mod lexer;
use lexer::Lexer;

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar
);

pub struct Parser<'ast, 'input> {
    pub arena: &'ast AstArena<'ast, 'input>,
    pub errors: Vec<Error>,
}

impl<'ast, 'input> Parser<'ast, 'input> {
    pub fn new(arena: &'ast AstArena<'ast, 'input>) -> Self {
        Parser {
            arena,
            errors: Vec::with_capacity(0),
        }
    }

    pub fn alloc<T: ArenaAlloc<'ast, 'input>>(&self, value: T) -> &'ast T {
        value.alloc_in(self.arena)
    }

    pub fn parse_program(&mut self, input: &'input str) -> ast::Program<'ast, 'input> {
        grammar::ProgramParser::new()
            .parse(self, Lexer::new(input))
            .expect("`Program` rule recovers from all errors")
    }

    pub fn parse_expr(&mut self, input: &'input str) -> ast::Expr<'ast, 'input> {
        grammar::EndExprParser::new()
            .parse(self, Lexer::new(input))
            .expect("`EndExpr` rule recovers from all errors")
    }
}

pub trait ArenaAlloc<'ast, 'input>: Sized {
    fn alloc_in(self, arena: &'ast AstArena<'ast, 'input>) -> &'ast Self;
}

impl<'ast, 'input> ArenaAlloc<'ast, 'input> for ast::Expr<'ast, 'input> {
    fn alloc_in(self, arena: &'ast AstArena<'ast, 'input>) -> &'ast Self {
        arena.exprs.alloc(self)
    }
}

impl<'ast, 'input> ArenaAlloc<'ast, 'input> for ast::Closure<'ast, 'input> {
    fn alloc_in(self, arena: &'ast AstArena<'ast, 'input>) -> &'ast Self {
        arena.closures.alloc(self)
    }
}

impl<'ast, 'input> ArenaAlloc<'ast, 'input> for ast::Pat<'ast, 'input> {
    fn alloc_in(self, arena: &'ast AstArena<'ast, 'input>) -> &'ast Self {
        arena.pats.alloc(self)
    }
}

impl<'ast, 'input> ArenaAlloc<'ast, 'input> for ast::Type<'ast, 'input> {
    fn alloc_in(self, arena: &'ast AstArena<'ast, 'input>) -> &'ast Self {
        arena.types.alloc(self)
    }
}
