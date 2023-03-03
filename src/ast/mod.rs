use std::fmt;

pub enum Expr<'ast, 'input> {
    Symbol(Symbol),
    Lit(Lit<'input>),
    Tuple(Vec<&'ast Expr<'ast, 'input>>),
    Closure(Closure<'ast, 'input>),
    Appl(Appl<'ast, 'input>),
}

impl fmt::Debug for Expr<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbol(arg0) => arg0.fmt(f),
            Self::Lit(arg0) => arg0.fmt(f),
            Self::Tuple(arg0) => arg0.fmt(f),
            Self::Closure(arg0) => arg0.fmt(f),
            Self::Appl(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum Lit<'input> {
    Integer(i32),
    Ident(Ident<'input>),
}

pub struct Ident<'input> {
    pub inner: &'input str,
}

impl fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<'input> Ident<'input> {
    pub fn new(inner: &'input str) -> Self {
        Ident { inner }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Symbol {
    Unit,
    Plus,
    Minus,
    Times,
    DotDot,
    Semi,
}

pub enum Closure<'ast, 'input> {
    Nonpiecewise(IrrefutableClosure<'ast, 'input>),
    Piecewise(Vec<RefutableClosure<'ast, 'input>>),
}

#[derive(Debug)]
pub struct ClosureKind<'ast, 'input, Item> {
    pub params: Params<Item>,
    pub body: &'ast Expr<'ast, 'input>,
}

pub type IrrefutableClosure<'ast, 'input> = ClosureKind<'ast, 'input, Ident<'input>>;
pub type RefutableClosure<'ast, 'input> = ClosureKind<'ast, 'input, Lit<'input>>;

#[derive(Debug)]
pub enum Pat<Item> {
    Item(Item),
    Tuple(Vec<Pat<Item>>),
}

#[derive(Debug)]
pub enum Params<Item> {
    Zero,
    One(Pat<Item>),
    Two(Pat<Item>, Pat<Item>),
}

#[derive(Debug)]
pub struct Appl<'ast, 'input> {
    pub left: &'ast Expr<'ast, 'input>,
    pub function: &'ast Expr<'ast, 'input>,
    pub right: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> Appl<'ast, 'input> {
    pub fn new(
        left: &'ast Expr<'ast, 'input>,
        function: &'ast Expr<'ast, 'input>,
        right: &'ast Expr<'ast, 'input>,
    ) -> Self {
        Appl {
            left,
            function,
            right,
        }
    }
}

impl<'ast, 'input, Item> ClosureKind<'ast, 'input, Item> {
    pub fn zero(body: &'ast Expr<'ast, 'input>) -> Self {
        ClosureKind {
            params: Params::Zero,
            body,
        }
    }

    pub fn one(p1: Pat<Item>, body: &'ast Expr<'ast, 'input>) -> Self {
        ClosureKind {
            params: Params::One(p1),
            body,
        }
    }

    pub fn two(
        p1: Pat<Item>,
        p2: Pat<Item>,
        body: &'ast Expr<'ast, 'input>,
    ) -> Self {
        ClosureKind {
            params: Params::Two(p1, p2),
            body,
        }
    }
}
