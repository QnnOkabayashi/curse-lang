mod expr;
mod pat;
pub mod tok;
mod ty;

pub use expr::*;
pub use pat::*;
pub use ty::*;

#[derive(Clone, Debug)]
pub struct Program<'ast, 'input> {
    pub items: Vec<Item<'ast, 'input>>,
}

impl<'ast, 'input> Program<'ast, 'input> {
    pub fn new(items: Vec<Option<Item<'ast, 'input>>>) -> Option<Self> {
        Some(Program {
            items: items.into_iter().collect::<Option<_>>()?,
        })
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
        typ: Option<&'ast Type<'ast, 'input>>,
        equal: tok::Equal,
        closure: Option<ExprClosure<'ast, 'input>>,
    ) -> Option<Self> {
        Some(ItemFunction {
            fn_,
            name,
            colon,
            typ: typ?,
            equal,
            closure: closure?,
        })
    }
}
