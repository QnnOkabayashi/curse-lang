use crate::lex::tok;

pub mod types;

pub struct Arena<'ast, 'input> {
    exprs: typed_arena::Arena<Expr<'ast, 'input>>,
}

impl Default for Arena<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast, 'input> Arena<'ast, 'input> {
    pub fn new() -> Self {
        Arena {
            exprs: typed_arena::Arena::with_capacity(1024),
        }
    }

    pub fn expr(&'ast self, expr: Expr<'ast, 'input>) -> &'ast Expr<'ast, 'input> {
        self.exprs.alloc(expr)
    }
}

#[derive(Clone, Debug)]
pub struct Program<'ast, 'input> {
    pub items: Vec<Item<'ast, 'input>>,
}

impl<'ast, 'input> Program<'ast, 'input> {
    pub fn new(items: Vec<Item<'ast, 'input>>) -> Self {
        Program { items }
    }
}

#[derive(Clone, Debug)]
pub enum Item<'ast, 'input> {
    Function(ItemFunction<'ast, 'input>),
}

#[derive(Clone, Debug)]
pub struct ItemFunction<'ast, 'input> {
    pub tok_fn: tok::Fn,
    pub name: tok::Ident<'input>,
    pub tok_colon: tok::Colon,
    pub typ: types::Type<'input>,
    pub tok_equal: tok::Equal,
    pub closure: Closure<'ast, 'input>,
}

impl<'ast, 'input> ItemFunction<'ast, 'input> {
    pub fn new(
        tok_fn: tok::Fn,
        name: tok::Ident<'input>,
        tok_colon: tok::Colon,
        typ: types::Type<'input>,
        tok_equal: tok::Equal,
        closure: Closure<'ast, 'input>,
    ) -> Self {
        ItemFunction {
            tok_fn,
            name,
            tok_colon,
            typ,
            tok_equal,
            closure,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr<'ast, 'input> {
    Paren(Paren<'ast, 'input>),
    Symbol(Symbol),
    Lit(Lit<'input>),
    Tuple(Tuple<&'ast Expr<'ast, 'input>>),
    Closure(Closure<'ast, 'input>),
    Appl(Appl<'ast, 'input>),
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
    Unit(tok::LParen, tok::RParen),
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
pub struct Tuple<T> {
    pub lparen: tok::LParen,
    pub elements: Vec<(T, tok::Comma)>,
    pub trailing: Option<Box<T>>,
    pub rparen: tok::RParen,
}

impl<T> Tuple<T> {
    pub fn new(
        lparen: tok::LParen,
        elements: Vec<(T, tok::Comma)>,
        trailing: Option<T>,
        rparen: tok::RParen,
    ) -> Self {
        Tuple {
            lparen,
            elements,
            trailing: trailing.map(Box::new),
            rparen,
        }
    }

    pub fn iter_elements(&self) -> impl Iterator<Item = &T> {
        self.elements
            .iter()
            .map(|(elem, _)| elem)
            .chain(self.trailing.as_deref())
    }

    pub fn len(&self) -> usize {
        self.elements.len() + if self.trailing.is_some() { 1 } else { 0 }
    }
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
    pub params: Params<'input>,
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
        lhs: Pat<'input>,
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
        lhs: Pat<'input>,
        rhs: Pat<'input>,
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
pub enum Pat<'input> {
    Lit(Lit<'input>),
    Tuple(Tuple<Pat<'input>>),
}

#[derive(Clone, Debug)]
pub enum Params<'input> {
    Zero,
    One(Pat<'input>),
    Two(Pat<'input>, Pat<'input>),
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
