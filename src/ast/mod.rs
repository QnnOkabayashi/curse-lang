pub struct Arena<'ast, 'input> {
    arena: typed_arena::Arena<Expr<'ast, 'input>>,
}

impl<'ast, 'input> Arena<'ast, 'input> {
    pub fn new() -> Self {
        Arena {
            arena: typed_arena::Arena::with_capacity(1024),
        }
    }

    pub fn symbol(&'ast self, symbol: Symbol) -> &'ast Expr<'ast, 'input> {
        self.arena.alloc(Expr::Symbol(symbol))
    }

    pub fn lit(&'ast self, lit: Lit<'input>) -> &'ast Expr<'ast, 'input> {
        self.arena.alloc(Expr::Lit(lit))
    }

    pub fn tuple(&'ast self, vec: Vec<&'ast Expr<'ast, 'input>>) -> &'ast Expr<'ast, 'input> {
        self.arena.alloc(Expr::Tuple(vec))
    }

    pub fn closure(&'ast self, closure: Closure<'ast, 'input>) -> &'ast Expr<'ast, 'input> {
        self.arena.alloc(Expr::Closure(closure))
    }

    pub fn appl(&'ast self, appl: Appl<'ast, 'input>) -> &'ast Expr<'ast, 'input> {
        self.arena.alloc(Expr::Appl(appl))
    }
}

#[derive(Debug)]
pub enum Expr<'ast, 'input> {
    Symbol(Symbol),
    Lit(Lit<'input>),
    Tuple(Vec<&'ast Expr<'ast, 'input>>),
    Closure(Closure<'ast, 'input>),
    Appl(Appl<'ast, 'input>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Lit<'input> {
    Integer(i32),
    Ident(Ident<'input>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident<'input> {
    pub inner: &'input str,
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

#[derive(Debug)]
pub struct Closure<'ast, 'input> {
    pub branches: Vec<Branch<'ast, 'input>>,
}

impl<'ast, 'input> Closure<'ast, 'input> {
    pub fn new(branch: Branch<'ast, 'input>) -> Self {
        Closure {
            branches: vec![branch],
        }
    }

    pub fn with_branch(mut self, branch: Branch<'ast, 'input>) -> Self {
        self.branches.push(branch);
        self
    }
}

#[derive(Debug)]
pub struct Branch<'ast, 'input> {
    pub params: Params<'input>,
    pub body: &'ast Expr<'ast, 'input>,
}

#[derive(Debug)]
pub enum Pat<'input> {
    Lit(Lit<'input>),
    Tuple(Vec<Pat<'input>>),
}

#[derive(Debug)]
pub enum Params<'input> {
    Zero,
    One(Pat<'input>),
    Two(Pat<'input>, Pat<'input>),
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

impl<'ast, 'input> Branch<'ast, 'input> {
    pub fn zero(body: &'ast Expr<'ast, 'input>) -> Self {
        Branch {
            params: Params::Zero,
            body,
        }
    }

    pub fn one(p1: Pat<'input>, body: &'ast Expr<'ast, 'input>) -> Self {
        Branch {
            params: Params::One(p1),
            body,
        }
    }

    pub fn two(p1: Pat<'input>, p2: Pat<'input>, body: &'ast Expr<'ast, 'input>) -> Self {
        Branch {
            params: Params::Two(p1, p2),
            body,
        }
    }
}
