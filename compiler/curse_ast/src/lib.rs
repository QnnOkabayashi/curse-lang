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
    pub fn new(item: Item<'ast, 'input>) -> Self {
        Program { items: vec![item] }
    }

    pub fn with_item(mut self, item: Item<'ast, 'input>) -> Self {
        self.items.push(item);
        self
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
