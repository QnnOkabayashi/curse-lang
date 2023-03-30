use crate::{pat, tok, ty};

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

#[derive(Copy, Clone, Debug)]
pub enum ExprLit<'input> {
    Integer(tok::Integer<'input>),
    Ident(tok::Ident<'input>),
    True(tok::True),
    False(tok::False),
}

#[derive(Clone, Debug)]
pub struct ExprClosure<'ast, 'input> {
    pub head: ExprBranch<'ast, 'input>,
    pub tail: Vec<(tok::Else, ExprBranch<'ast, 'input>)>,
}

impl<'ast, 'input> ExprClosure<'ast, 'input> {
    pub fn new(head: ExprBranch<'ast, 'input>) -> Self {
        ExprClosure { head, tail: vec![] }
    }

    pub fn with_branch(mut self, els: tok::Else, branch: ExprBranch<'ast, 'input>) -> Self {
        self.tail.push((els, branch));
        self
    }

    pub fn iter_branches(&self) -> impl Iterator<Item = &ExprBranch<'ast, 'input>> {
        Some(&self.head)
            .into_iter()
            .chain(self.tail.iter().map(|(_, branch)| branch))
    }
}

#[derive(Clone, Debug)]
pub struct ExprBranch<'ast, 'input> {
    pub open: tok::Pipe,
    pub params: ExprParams<'ast, 'input>,
    pub close: tok::Pipe,
    pub body: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> ExprBranch<'ast, 'input> {
    pub fn new(
        open: tok::Pipe,
        params: ExprParams<'ast, 'input>,
        close: tok::Pipe,
        body: &'ast Expr<'ast, 'input>,
    ) -> Self {
        ExprBranch {
            open,
            params,
            close,
            body,
        }
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
