#![forbid(unsafe_code)]

use curse_ast::ast;
use curse_interner::StringInterner;
use lalrpop_util::lalrpop_mod;

mod error;
pub use error::Error;

mod lexer;
use lexer::Lexer;

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar
);

pub struct Parser<'intern> {
    pub interner: &'intern mut StringInterner,
    pub errors: Vec<Error>,
}

impl<'intern> Parser<'intern> {
    pub fn new(interner: &'intern mut StringInterner) -> Self {
        Parser {
            interner,
            errors: Vec::with_capacity(0),
        }
    }

    pub fn parse_program(&mut self, input: &str) -> ast::Program {
        grammar::ProgramParser::new()
            .parse(self, Lexer::new(input))
            .expect("`Program` rule recovers from all errors")
    }

    pub fn parse_expr(&mut self, input: &str) -> ast::Expr {
        grammar::EndExprParser::new()
            .parse(self, Lexer::new(input))
            .expect("`EndExpr` rule recovers from all errors")
    }
}
