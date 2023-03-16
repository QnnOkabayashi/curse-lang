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
    pub inner: &'ast Expr<'ast, 'input>,
    pub rparen: tok::RParen,
}

impl<'ast, 'input> ExprParen<'ast, 'input> {
    pub fn new(
        lparen: tok::LParen,
        inner: Option<&'ast Expr<'ast, 'input>>,
        rparen: tok::RParen,
    ) -> Option<Self> {
        inner.map(|inner| ExprParen {
            lparen,
            inner,
            rparen,
        })
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ExprSymbol {
    Plus(tok::Plus),
    Minus(tok::Minus),
    Star(tok::Star),
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
    pub branches: Vec<(ExprBranch<'ast, 'input>, tok::Else)>,
    pub last: ExprBranch<'ast, 'input>,
}

impl<'ast, 'input> ExprClosure<'ast, 'input> {
    pub fn new(
        branches: Vec<(Option<ExprBranch<'ast, 'input>>, tok::Else)>,
        last: Option<ExprBranch<'ast, 'input>>,
    ) -> Option<Self> {
        // This is pain.
        Some(ExprClosure {
            last: last?,
            branches: branches
                .into_iter()
                .map(|(branch, els)| branch.map(|branch| (branch, els)))
                .collect::<Option<_>>()?,
        })
    }

    pub fn iter_branches(&self) -> impl Iterator<Item = &ExprBranch<'ast, 'input>> {
        self.branches
            .iter()
            .map(|(branch, _)| branch)
            .chain(Some(&self.last))
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
    pub fn zero(
        open: tok::Pipe,
        close: tok::Pipe,
        body: Option<&'ast Expr<'ast, 'input>>,
    ) -> Option<Self> {
        Some(ExprBranch {
            open,
            params: ExprParams::Zero,
            close,
            body: body?,
        })
    }

    pub fn one(
        open: tok::Pipe,
        lhs: ExprParam<'ast, 'input>,
        close: tok::Pipe,
        body: Option<&'ast Expr<'ast, 'input>>,
    ) -> Option<Self> {
        Some(ExprBranch {
            open,
            params: ExprParams::One(lhs),
            close,
            body: body?,
        })
    }

    pub fn two(
        open: tok::Pipe,
        lhs: ExprParam<'ast, 'input>,
        comma: tok::Comma,
        rhs: ExprParam<'ast, 'input>,
        close: tok::Pipe,
        body: Option<&'ast Expr<'ast, 'input>>,
    ) -> Option<Self> {
        Some(ExprBranch {
            open,
            params: ExprParams::Two(lhs, comma, rhs),
            close,
            body: body?,
        })
    }
}

#[derive(Clone, Debug)]
pub enum ExprParams<'ast, 'input> {
    Zero,
    One(ExprParam<'ast, 'input>),
    Two(ExprParam<'ast, 'input>, tok::Comma, ExprParam<'ast, 'input>),
}

#[derive(Clone, Debug)]
pub struct ExprParam<'ast, 'input> {
    pub pat: &'ast ExprPat<'ast, 'input>,
    pub ty: Option<(tok::Colon, &'ast ty::Type<'ast, 'input>)>,
}

impl<'ast, 'input> ExprParam<'ast, 'input> {
    pub fn new(
        pat: &'ast ExprPat<'ast, 'input>,
        ty: Option<(tok::Colon, Option<&'ast ty::Type<'ast, 'input>>)>,
    ) -> Self {
        ExprParam {
            pat,
            ty: ty.and_then(|(colon, ty)| Some((colon, ty?))),
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
    pub fn new(
        lhs: Option<&'ast Expr<'ast, 'input>>,
        function: Option<&'ast Expr<'ast, 'input>>,
        rhs: Option<&'ast Expr<'ast, 'input>>,
    ) -> Option<Self> {
        Some(ExprAppl {
            lhs: lhs?,
            function: function?,
            rhs: rhs?,
        })
    }
}
