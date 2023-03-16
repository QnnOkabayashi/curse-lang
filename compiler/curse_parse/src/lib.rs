use curse_ast::{Expr, ExprPat, Program, Type};
use lalrpop_util::lalrpop_mod;
use typed_arena::Arena;

mod error;
pub use error::{Error, SourceErrors};

mod lexer;
use lexer::Lexer;

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar
);

pub struct Context<'ast, 'input> {
    exprs: Arena<Expr<'ast, 'input>>,
    pats: Arena<ExprPat<'ast, 'input>>,
    types: Arena<Type<'ast, 'input>>,
}

impl Default for Context<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast, 'input> Context<'ast, 'input> {
    pub fn new() -> Self {
        Context {
            exprs: Arena::with_capacity(1024),
            pats: Arena::with_capacity(1024),
            types: Arena::with_capacity(1024),
        }
    }

    pub fn expr(&'ast self, expr: Expr<'ast, 'input>) -> &'ast Expr<'ast, 'input> {
        self.exprs.alloc(expr)
    }

    pub fn pat(&'ast self, pat: ExprPat<'ast, 'input>) -> &'ast ExprPat<'ast, 'input> {
        self.pats.alloc(pat)
    }

    pub fn typ(&'ast self, typ: Type<'ast, 'input>) -> &'ast Type<'ast, 'input> {
        self.types.alloc(typ)
    }
}

pub fn parse_program<'ast, 'input>(
    context: &'ast Context<'ast, 'input>,
    input: &'input str,
) -> Result<Program<'ast, 'input>, Vec<Error>> {
    let mut errors = Vec::with_capacity(0);
    match grammar::ProgramParser::new().parse(context, &mut errors, Lexer::new(input)) {
        Ok(Some(program)) => Ok(program),
        Ok(None) => Err(errors),
        Err(err) => {
            errors.push(err.into());
            Err(errors)
        }
    }
}

pub fn parse_expr<'ast, 'input>(
    context: &'ast Context<'ast, 'input>,
    input: &'input str,
) -> Result<&'ast Expr<'ast, 'input>, Vec<Error>> {
    let mut errors = Vec::with_capacity(0);
    match grammar::EndExprParser::new().parse(context, &mut errors, Lexer::new(input)) {
        Ok(Some(expr)) => Ok(expr),
        Ok(None) => Err(errors),
        Err(err) => {
            errors.push(err.into());
            Err(errors)
        }
    }
}
