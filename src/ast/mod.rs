use crate::lex::tok;

pub mod expr;
pub mod pat;
pub mod ty;

pub struct Arena<'ast, 'input> {
    exprs: typed_arena::Arena<expr::Expr<'ast, 'input>>,
    pats: typed_arena::Arena<expr::Pat<'ast, 'input>>,
    types: typed_arena::Arena<ty::Type<'ast, 'input>>,
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
            pats: typed_arena::Arena::with_capacity(1024),
            types: typed_arena::Arena::with_capacity(1024),
        }
    }

    pub fn expr(&'ast self, expr: expr::Expr<'ast, 'input>) -> &'ast expr::Expr<'ast, 'input> {
        self.exprs.alloc(expr)
    }

    pub fn pat(&'ast self, pat: expr::Pat<'ast, 'input>) -> &'ast expr::Pat<'ast, 'input> {
        self.pats.alloc(pat)
    }

    pub fn typ(&'ast self, typ: ty::Type<'ast, 'input>) -> &'ast ty::Type<'ast, 'input> {
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
    pub tok_fn: tok::Fn,
    pub name: tok::Ident<'input>,
    pub tok_colon: tok::Colon,
    pub typ: &'ast ty::Type<'ast, 'input>,
    pub tok_equal: tok::Equal,
    pub closure: expr::Closure<'ast, 'input>,
}

impl<'ast, 'input> ItemFunction<'ast, 'input> {
    pub fn new(
        tok_fn: tok::Fn,
        name: tok::Ident<'input>,
        tok_colon: tok::Colon,
        typ: &'ast ty::Type<'ast, 'input>,
        tok_equal: tok::Equal,
        closure: expr::Closure<'ast, 'input>,
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
