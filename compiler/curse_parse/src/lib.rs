#![forbid(unsafe_code)]

use bumpalo::Bump;
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

pub struct Parser<'ast> {
    pub bump: &'ast Bump,
    pub strings: &'ast Bump,
    pub errors: Vec<Error>,
}

impl<'ast> Parser<'ast> {
    pub fn new(bump: &'ast Bump, strings: &'ast Bump) -> Self {
        Parser {
            bump,
            strings,
            errors: Vec::with_capacity(0),
        }
    }

    pub fn parse_program(&mut self, input: &str) -> ast::Program<'ast> {
        grammar::ProgramParser::new()
            .parse(self, Lexer::new(self.strings, input))
            .expect("`Program` rule recovers from all errors")
    }

    pub fn parse_expr(&mut self, input: &str) -> ast::Expr<'ast> {
        grammar::EndExprParser::new()
            .parse(self, Lexer::new(self.strings, input))
            .expect("`EndExpr` rule recovers from all errors")
    }
}
