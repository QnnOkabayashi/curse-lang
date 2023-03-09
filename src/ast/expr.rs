use crate::ast::pat;
use crate::lex::{tok, LexError, Token};
use lalrpop_util::ErrorRecovery;

#[derive(Clone, Debug)]
pub enum Expr<'ast, 'input> {
    Paren(Paren<'ast, 'input>),
    Symbol(Symbol),
    Lit(Lit<'input>),
    Tuple(pat::Tuple<&'ast Expr<'ast, 'input>>),
    Closure(Closure<'ast, 'input>),
    Appl(Appl<'ast, 'input>),
    Error(ErrorRecovery<usize, Token<'input>, LexError>),
}

#[derive(Copy, Clone, Debug)]
pub struct Paren<'ast, 'input> {
    pub lparen: tok::LParen,
    pub inner: &'ast Expr<'ast, 'input>,
    pub rparen: tok::RParen,
}

impl<'ast, 'input> Paren<'ast, 'input> {
    pub fn new(lparen: tok::LParen, inner: &'ast Expr<'ast, 'input>, rparen: tok::RParen) -> Self {
        Paren {
            lparen,
            inner,
            rparen,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Symbol {
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
pub enum Lit<'input> {
    Integer(tok::Integer<'input>),
    Ident(tok::Ident<'input>),
}

#[derive(Clone, Debug)]
pub struct Closure<'ast, 'input> {
    pub branches: Vec<(Branch<'ast, 'input>, tok::Else)>,
    pub last: Branch<'ast, 'input>,
}

impl<'ast, 'input> Closure<'ast, 'input> {
    pub fn new(
        branches: Vec<(Branch<'ast, 'input>, tok::Else)>,
        last: Branch<'ast, 'input>,
    ) -> Self {
        Closure { branches, last }
    }

    pub fn iter_branches(&self) -> impl Iterator<Item = &Branch<'ast, 'input>> {
        self.branches
            .iter()
            .map(|(branch, _)| branch)
            .chain(Some(&self.last))
    }
}

#[derive(Clone, Debug)]
pub struct Branch<'ast, 'input> {
    pub open: tok::Pipe,
    pub params: Params<'ast, 'input>,
    pub close: tok::Pipe,
    pub body: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> Branch<'ast, 'input> {
    pub fn zero(open: tok::Pipe, close: tok::Pipe, body: &'ast Expr<'ast, 'input>) -> Self {
        Branch {
            open,
            params: Params::Zero,
            close,
            body,
        }
    }

    pub fn one(
        open: tok::Pipe,
        lhs: &'ast Pat<'ast, 'input>,
        close: tok::Pipe,
        body: &'ast Expr<'ast, 'input>,
    ) -> Self {
        Branch {
            open,
            params: Params::One(lhs),
            close,
            body,
        }
    }

    pub fn two(
        open: tok::Pipe,
        lhs: &'ast Pat<'ast, 'input>,
        rhs: &'ast Pat<'ast, 'input>,
        close: tok::Pipe,
        body: &'ast Expr<'ast, 'input>,
    ) -> Self {
        Branch {
            open,
            params: Params::Two(lhs, rhs),
            close,
            body,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Pat<'ast, 'input> {
    Lit(Lit<'input>),
    Tuple(pat::Tuple<&'ast Pat<'ast, 'input>>),
}

#[derive(Clone, Debug)]
pub enum Params<'ast, 'input> {
    Zero,
    One(&'ast Pat<'ast, 'input>),
    Two(&'ast Pat<'ast, 'input>, &'ast Pat<'ast, 'input>),
}

#[derive(Copy, Clone, Debug)]
pub struct Appl<'ast, 'input> {
    pub lhs: &'ast Expr<'ast, 'input>,
    pub function: &'ast Expr<'ast, 'input>,
    pub rhs: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> Appl<'ast, 'input> {
    pub fn new(
        lhs: &'ast Expr<'ast, 'input>,
        function: &'ast Expr<'ast, 'input>,
        rhs: &'ast Expr<'ast, 'input>,
    ) -> Self {
        Appl { lhs, function, rhs }
    }
}
