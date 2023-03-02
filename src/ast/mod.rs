#[derive(Debug)]
pub enum Expr<'ast, 'input> {
    Lit(Lit<'input>),
    Closure(Closure<'ast, 'input>),
    Appl(Appl<'ast, 'input>),
}

impl<'ast, 'input> Expr<'ast, 'input> {
    pub fn appl(
        left: &'ast Expr<'ast, 'input>,
        function: &'ast Expr<'ast, 'input>,
        right: &'ast Expr<'ast, 'input>,
    ) -> Self {
        Self::Appl(Appl {
            left,
            function,
            right,
        })
    }
}

#[derive(Debug)]
pub enum Lit<'input> {
    Symbol(Symbol),
    Integer(i32),
    Ident(&'input str),
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
pub struct Closure<'ast, 'input> {
    params: Params<'input>,
    body: &'ast Expr<'ast, 'input>,
}

#[derive(Debug)]
pub enum Params<'input> {
    Zero,
    One(Pat<'input>),
    Two(Pat<'input>, Pat<'input>),
}

#[derive(Debug)]
pub struct Appl<'ast, 'input> {
    left: &'ast Expr<'ast, 'input>,
    function: &'ast Expr<'ast, 'input>,
    right: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> Closure<'ast, 'input> {
    pub fn zero(body: &'ast Expr<'ast, 'input>) -> Self {
        Self {
            params: Params::Zero,
            body,
        }
    }

    pub fn one(p1: Pat<'input>, body: &'ast Expr<'ast, 'input>) -> Self {
        Self {
            params: Params::One(p1),
            body,
        }
    }

    pub fn two(p1: Pat<'input>, p2: Pat<'input>, body: &'ast Expr<'ast, 'input>) -> Self {
        Self {
            params: Params::Two(p1, p2),
            body,
        }
    }
}

#[derive(Debug)]
pub enum Pat<'input> {
    Ident(&'input str),
    Tuple(Vec<Pat<'input>>),
}
