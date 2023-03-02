#[derive(Debug)]
pub enum Expr<'ast, 'input> {
    Lit(Lit<'input>),
    Closure(Closure<'ast, 'input>),
    Appl(Appl<'ast, 'input>),
}

#[derive(Debug)]
pub enum Lit<'input> {
    Symbol(Symbol),
    Integer(i32),
    Ident(Ident<'input>),
}

#[derive(Debug)]
pub struct Ident<'input> {
    inner: &'input str,
}

impl<'input> Ident<'input> {
    pub fn new(inner: &'input str) -> Self {
        Ident { inner }
    }
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
    One(Pat<Ident<'input>>),
    Two(Pat<Ident<'input>>, Pat<Ident<'input>>),
}

#[derive(Debug)]
pub struct Appl<'ast, 'input> {
    left: &'ast Expr<'ast, 'input>,
    function: &'ast Expr<'ast, 'input>,
    right: &'ast Expr<'ast, 'input>,
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

impl<'ast, 'input> Closure<'ast, 'input> {
    pub fn zero(body: &'ast Expr<'ast, 'input>) -> Self {
        Closure {
            params: Params::Zero,
            body,
        }
    }

    pub fn one(p1: Pat<Ident<'input>>, body: &'ast Expr<'ast, 'input>) -> Self {
        Closure {
            params: Params::One(p1),
            body,
        }
    }

    pub fn two(p1: Pat<Ident<'input>>, p2: Pat<Ident<'input>>, body: &'ast Expr<'ast, 'input>) -> Self {
        Closure {
            params: Params::Two(p1, p2),
            body,
        }
    }
}

#[derive(Debug)]
pub enum Pat<Item> {
    Item(Item),
    Tuple(Vec<Pat<Item>>),
}
