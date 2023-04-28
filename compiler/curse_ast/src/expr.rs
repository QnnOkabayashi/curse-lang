use crate::{pat, tok, ty, Span};

pub type ExprPat<'ast, 'input> = pat::Pat<'ast, ExprLit<'input>>;
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
    Equal(tok::Equal),
    Less(tok::Less),
    Greater(tok::Greater),
    LessEqual(tok::LessEqual),
    GreaterEqual(tok::GreaterEqual),
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
            ExprSymbol::Equal(equal) => equal.span(),
            ExprSymbol::Less(less) => less.span(),
            ExprSymbol::Greater(greater) => greater.span(),
            ExprSymbol::LessEqual(less_equal) => less_equal.span(),
            ExprSymbol::GreaterEqual(greater_equal) => greater_equal.span(),
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
            ExprLit::True(t) => t.span(),
            ExprLit::False(f) => f.span(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprClosure<'ast, 'input> {
    NonPiecewise(ExprArm<'ast, 'input>),
    Piecewise {
        lbrace: tok::LBrace,
        head: ExprArm<'ast, 'input>,
        tail: Vec<(tok::Comma, ExprArm<'ast, 'input>)>,
        trailing_comma: Option<tok::Comma>,
        rbrace: tok::RBrace,
    },
}

impl Span for ExprClosure<'_, '_> {
    fn span(&self) -> (usize, usize) {
        match self {
            ExprClosure::NonPiecewise(arm) => arm.span(),
            ExprClosure::Piecewise { lbrace, rbrace, .. } => lbrace.span_between(rbrace),
        }
    }
}

impl<'ast, 'input> ExprClosure<'ast, 'input> {
    pub fn new_non_piecewise(arm: ExprArm<'ast, 'input>) -> Self {
        ExprClosure::NonPiecewise(arm)
    }

    pub fn new_piecewise(
        lbrace: tok::LBrace,
        head: ExprArm<'ast, 'input>,
        tail: Vec<(tok::Comma, ExprArm<'ast, 'input>)>,
        trailing_comma: Option<tok::Comma>,
        rbrace: tok::RBrace,
    ) -> Self {
        ExprClosure::Piecewise {
            lbrace,
            head,
            tail,
            trailing_comma,
            rbrace,
        }
    }

    pub fn num_branches(&self) -> usize {
        match self {
            ExprClosure::NonPiecewise(_) => 1,
            ExprClosure::Piecewise { tail, .. } => 1 + tail.len(),
        }
    }

    pub fn head(&self) -> &ExprArm<'ast, 'input> {
        match self {
            ExprClosure::NonPiecewise(head) => head,
            ExprClosure::Piecewise { head, .. } => head,
        }
    }

    pub fn tail(&self) -> Option<impl Iterator<Item = &ExprArm<'ast, 'input>>> {
        match self {
            ExprClosure::NonPiecewise(_) => None,
            ExprClosure::Piecewise { tail, .. } => Some(tail.iter().map(|(_comma, arm)| arm)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprArm<'ast, 'input> {
    pub open: tok::Pipe,
    pub params: ExprParams<'ast, 'input>,
    pub close: tok::Pipe,
    pub body: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> ExprArm<'ast, 'input> {
    pub fn new(
        open: tok::Pipe,
        params: ExprParams<'ast, 'input>,
        close: tok::Pipe,
        body: &'ast Expr<'ast, 'input>,
    ) -> Self {
        ExprArm {
            open,
            params,
            close,
            body,
        }
    }
}

impl Span for ExprArm<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.open.span_between(self.body)
    }
}

#[derive(Clone, Debug)]
pub enum ExprParams<'ast, 'input> {
    Zero,
    One(ExprParam<'ast, 'input>),
    Two(ExprParam<'ast, 'input>, tok::Comma, ExprParam<'ast, 'input>),
}

impl<'ast, 'input> ExprParams<'ast, 'input> {
    pub fn map<'a, T, F, D>(&'a self, mut f: F, mut default: D) -> Option<(T, T)>
    where
        F: FnMut(&'a ExprParam<'ast, 'input>) -> Option<T>,
        D: FnMut() -> T,
    {
        match self {
            ExprParams::Zero => Some((default(), default())),
            ExprParams::One(lhs) => Some((f(lhs)?, default())),
            ExprParams::Two(lhs, _, rhs) => f(lhs).zip(f(rhs)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprParam<'ast, 'input> {
    pub pat: &'ast ExprPat<'ast, 'input>,
    pub ty: Option<(tok::Colon, &'ast ty::Type<'ast, 'input>)>,
}

impl<'ast, 'input> ExprParam<'ast, 'input> {
    pub fn new(
        pat: &'ast ExprPat<'ast, 'input>,
        ty: Option<(tok::Colon, &'ast ty::Type<'ast, 'input>)>,
    ) -> Self {
        ExprParam { pat, ty }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ExprAppl<'ast, 'input> {
    pub lhs: &'ast Expr<'ast, 'input>,
    pub function: &'ast Expr<'ast, 'input>,
    pub rhs: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> ExprAppl<'ast, 'input> {
    pub fn new(
        lhs: &'ast Expr<'ast, 'input>,
        function: &'ast Expr<'ast, 'input>,
        rhs: &'ast Expr<'ast, 'input>,
    ) -> Self {
        ExprAppl { lhs, function, rhs }
    }
}

impl Span for ExprAppl<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.lhs.span_between(self.rhs)
    }
}
