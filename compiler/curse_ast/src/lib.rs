mod expr;
mod pat;
pub mod tok;
mod ty;

pub use expr::*;
pub use pat::*;
pub use ty::*;

pub struct Context<'ast, 'input> {
    exprs: typed_arena::Arena<Expr<'ast, 'input>>,
    pats: typed_arena::Arena<ExprPat<'ast, 'input>>,
    types: typed_arena::Arena<Type<'ast, 'input>>,
}

impl Default for Context<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast, 'input> Context<'ast, 'input> {
    pub fn new() -> Self {
        Context {
            exprs: typed_arena::Arena::with_capacity(1024),
            pats: typed_arena::Arena::with_capacity(1024),
            types: typed_arena::Arena::with_capacity(1024),
        }
    }

    pub fn expr(&'ast self, expr: Expr<'ast, 'input>) -> &'ast Expr<'ast, 'input> {
        self.exprs.alloc(expr)
    }

    pub fn pat(&'ast self, pat: ExprPat<'ast, 'input>) -> &'ast ExprPat<'ast, 'input> {
        self.pats.alloc(pat)
    }

    pub fn typ(&'ast self, typ: Type<'ast, 'input>) -> &'ast Type<'ast, 'input> {
        self.types.alloc(typ)
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
    pub fn_: tok::Fn,
    pub name: tok::Ident<'input>,
    pub colon: tok::Colon,
    pub typ: &'ast Type<'ast, 'input>,
    pub equal: tok::Equal,
    pub closure: ExprClosure<'ast, 'input>,
}

impl<'ast, 'input> ItemFunction<'ast, 'input> {
    pub fn new(
        fn_: tok::Fn,
        name: tok::Ident<'input>,
        colon: tok::Colon,
        typ: &'ast Type<'ast, 'input>,
        equal: tok::Equal,
        closure: ExprClosure<'ast, 'input>,
    ) -> Self {
        ItemFunction {
            fn_,
            name,
            colon,
            typ,
            equal,
            closure,
        }
    }
}
