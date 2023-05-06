use crate::{pat, tok, ParseError, Res, Span};

mod closure;
pub use closure::*;

pub type ExprPat<'ast, 'input> = pat::Pat<'ast, 'input>;
pub type ExprTuple<'ast, 'input> = pat::PatTuple<&'ast Expr<'ast, 'input>>;

#[derive(Clone, Debug)]
pub enum Expr<'ast, 'input> {
    Paren(ExprParen<'ast, 'input>),
    Symbol(ExprSymbol),
    Lit(ExprLit<'input>),
    Tuple(ExprTuple<'ast, 'input>),
    Closure(ExprClosure<'ast, 'input>),
    Appl(ExprAppl<'ast, 'input>),
}

impl<'ast, 'input> Expr<'ast, 'input> {
    pub fn paren_from_grammar(
        lparen: tok::LParen,
        res_expr: Res<&'ast Expr<'ast, 'input>>,
        rparen: tok::RParen,
    ) -> Res<Self> {
        res_expr.map(|expr| {
            Expr::Paren(ExprParen {
                lparen,
                expr,
                rparen,
            })
        })
    }

    pub fn tuple_from_grammar(tuple: pat::PatTuple<Res<&'ast Expr<'ast, 'input>>>) -> Res<Self> {
        Ok(Expr::Tuple(pat::PatTuple {
            lparen: tuple.lparen,
            kind: tuple.kind.map(pat::TupleNonempty::transpose).transpose()?,
            rparen: tuple.rparen,
        }))
    }

    pub fn closure_from_grammar(res_closure: Res<ExprClosure<'ast, 'input>>) -> Res<Self> {
        res_closure.map(Expr::Closure)
    }

    pub fn appl_from_grammar(res_appl: Res<ExprAppl<'ast, 'input>>) -> Res<Self> {
        res_appl.map(Expr::Appl)
    }
}

impl Span for Expr<'_, '_> {
    fn span(&self) -> (usize, usize) {
        match self {
            Expr::Paren(paren) => paren.span(),
            Expr::Symbol(symbol) => symbol.span(),
            Expr::Lit(lit) => lit.span(),
            Expr::Tuple(tuple) => tuple.span(),
            Expr::Closure(closure) => closure.span(),
            Expr::Appl(appl) => appl.span(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ExprParen<'ast, 'input> {
    pub lparen: tok::LParen,
    pub expr: &'ast Expr<'ast, 'input>,
    pub rparen: tok::RParen,
}

impl<'ast, 'input> ExprParen<'ast, 'input> {
    pub fn new(lparen: tok::LParen, expr: &'ast Expr<'ast, 'input>, rparen: tok::RParen) -> Self {
        ExprParen {
            lparen,
            expr,
            rparen,
        }
    }
}

impl Span for ExprParen<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.lparen.span_between(self.rparen)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ExprSymbol {
    Plus(tok::Plus),
    Minus(tok::Minus),
    Star(tok::Star),
    Dot(tok::Dot),
    DotDot(tok::DotDot),
    Semi(tok::Semi),
    Percent(tok::Percent),
    Slash(tok::Slash),
    Eq(tok::Eq),
    Lt(tok::Lt),
    Gt(tok::Gt),
    Le(tok::Le),
    Ge(tok::Ge),
}

impl Span for ExprSymbol {
    fn span(&self) -> (usize, usize) {
        match self {
            ExprSymbol::Plus(plus) => plus.span(),
            ExprSymbol::Minus(minus) => minus.span(),
            ExprSymbol::Star(star) => star.span(),
            ExprSymbol::Dot(dot) => dot.span(),
            ExprSymbol::DotDot(dotdot) => dotdot.span(),
            ExprSymbol::Semi(semi) => semi.span(),
            ExprSymbol::Percent(percent) => percent.span(),
            ExprSymbol::Slash(slash) => slash.span(),
            ExprSymbol::Eq(equal) => equal.span(),
            ExprSymbol::Lt(less) => less.span(),
            ExprSymbol::Gt(greater) => greater.span(),
            ExprSymbol::Le(less_equal) => less_equal.span(),
            ExprSymbol::Ge(greater_equal) => greater_equal.span(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ExprLit<'input> {
    Integer(tok::Integer<'input>),
    Ident(tok::Ident<'input>),
    True(tok::True),
    False(tok::False),
}

impl Span for ExprLit<'_> {
    fn span(&self) -> (usize, usize) {
        match self {
            ExprLit::Integer(integer) => integer.span(),
            ExprLit::Ident(ident) => ident.span(),
            ExprLit::True(tru) => tru.span(),
            ExprLit::False(fals) => fals.span(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ExprAppl<'ast, 'input> {
    pub lhs: &'ast Expr<'ast, 'input>,
    pub function: &'ast Expr<'ast, 'input>,
    pub rhs: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> ExprAppl<'ast, 'input> {
    pub fn from_grammar(
        res_lhs: Res<&'ast Expr<'ast, 'input>>,
        res_function: Res<&'ast Expr<'ast, 'input>>,
        res_rhs: Res<&'ast Expr<'ast, 'input>>,
    ) -> Res<Self> {
        match (res_lhs, res_function, res_rhs) {
            (Ok(lhs), Ok(function), Ok(rhs)) => Ok(ExprAppl { lhs, function, rhs }),
            _ => Err(ParseError),
        }
    }
}

impl Span for ExprAppl<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.lhs.span_between(self.rhs)
    }
}
