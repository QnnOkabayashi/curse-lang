use curse_ast::{Expr, ExprPat, ParseError, Program, Res, Type};
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

pub struct Ast<'ast, 'input> {
    exprs: Arena<Expr<'ast, 'input>>,
    pats: Arena<ExprPat<'ast, 'input>>,
    types: Arena<Type<'ast, 'input>>,
}

impl Default for Ast<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast, 'input> Ast<'ast, 'input> {
    pub fn new() -> Self {
        Ast {
            exprs: Arena::with_capacity(1024),
            pats: Arena::with_capacity(1024),
            types: Arena::with_capacity(1024),
        }
    }

    pub fn expr(&'ast self, expr: Expr<'ast, 'input>) -> &'ast Expr<'ast, 'input> {
        self.exprs.alloc(expr)
    }

    pub fn try_expr(
        &'ast self,
        res_expr: Res<Expr<'ast, 'input>>,
    ) -> Res<&'ast Expr<'ast, 'input>> {
        res_expr.map(|expr| self.expr(expr))
    }

    pub fn pat(&'ast self, pat: ExprPat<'ast, 'input>) -> &'ast ExprPat<'ast, 'input> {
        self.pats.alloc(pat)
    }

    pub fn ty(&'ast self, ty: Type<'ast, 'input>) -> &'ast Type<'ast, 'input> {
        self.types.alloc(ty)
    }

    pub fn try_ty(&'ast self, res_ty: Res<Type<'ast, 'input>>) -> Res<&'ast Type<'ast, 'input>> {
        res_ty.map(|ty| self.ty(ty))
    }
}

pub fn parse_program<'ast, 'input>(
    ast: &'ast Ast<'ast, 'input>,
    input: &'input str,
) -> Result<Program<'ast, 'input>, Vec<Error>> {
    let mut errors = Vec::with_capacity(0);
    match grammar::ProgramParser::new().parse(ast, &mut errors, Lexer::new(input)) {
        Ok(Ok(program)) => Ok(program),
        Ok(Err(ParseError)) => Err(errors),
        Err(err) => {
            errors.push(err.into());
            Err(errors)
        }
    }
}

pub fn parse_expr<'ast, 'input>(
    context: &'ast Ast<'ast, 'input>,
    input: &'input str,
) -> Result<&'ast Expr<'ast, 'input>, Vec<Error>> {
    let mut errors = Vec::with_capacity(0);
    match grammar::EndExprParser::new().parse(context, &mut errors, Lexer::new(input)) {
        Ok(Ok(expr)) => Ok(expr),
        Ok(Err(ParseError)) => Err(errors),
        Err(err) => {
            errors.push(err.into());
            Err(errors)
        }
    }
}
