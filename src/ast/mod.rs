#[derive(Debug)]
pub enum Expr<'a> {
    Lit(Lit<'a>),
    Closure(Closure<'a>),
    Appl(Appl<'a>),
}

impl<'a> Expr<'a> {
    pub fn appl(left: Expr<'a>, function: Expr<'a>, right: Expr<'a>) -> Self {
        Self::Appl(Appl {
            left: Box::new(left),
            function: Box::new(function),
            right: Box::new(right),
        })
    }
}

#[derive(Debug)]
pub enum Lit<'a> {
    Symbol(Symbol),
    Integer(i32),
    Ident(&'a str),
}

#[derive(Debug)]
pub enum Symbol {
    Unit,
    Plus,
    Minus,
    Times,
    DotDot,
    Semi,
}

#[derive(Debug)]
pub struct Closure<'a> {
    pub params: Params<'a>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug)]
pub enum Params<'a> {
    Zero,
    One(Pat<'a>),
    Two(Pat<'a>, Pat<'a>),
}

#[derive(Debug)]
pub struct Appl<'a> {
    pub left: Box<Expr<'a>>,
    pub function: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
}

impl<'a> Closure<'a> {
    pub fn zero(body: Expr<'a>) -> Self {
        Self {
            params: Params::Zero,
            body: Box::new(body),
        }
    }

    pub fn one(p1: Pat<'a>, body: Expr<'a>) -> Self {
        Self {
            params: Params::One(p1),
            body: Box::new(body),
        }
    }

    pub fn two(p1: Pat<'a>, p2: Pat<'a>, body: Expr<'a>) -> Self {
        Self {
            params: Params::Two(p1, p2),
            body: Box::new(body),
        }
    }
}

#[derive(Debug)]
pub enum Pat<'a> {
    Ident(&'a str),
    Tuple(Vec<Pat<'a>>),
}
