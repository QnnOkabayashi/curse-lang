mod expr;
mod pat;
pub mod tok;
mod ty;

pub use expr::*;
pub use pat::*;
pub use ty::*;

pub trait Span {
    fn span(&self) -> (usize, usize);

    fn span_between(&self, other: impl Span) -> (usize, usize) {
        let (start1, len1) = self.span();
        let (start2, len2) = other.span();
        let start = std::cmp::min(start1, start2);
        let end = std::cmp::max(start1 + len1, start2 + len2);
        (start, end - start)
    }
}

impl<T: Span> Span for &T {
    fn span(&self) -> (usize, usize) {
        (*self).span()
    }
}

impl Span for (usize, usize) {
    fn span(&self) -> (usize, usize) {
        *self
    }
}

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
pub struct Item<'ast, 'input> {
    pub let_: tok::Let,
    pub name: tok::Ident<'input>,
    pub generics: Vec<tok::Ident<'input>>,
    pub colon: tok::Colon,
    pub typ: &'ast Type<'ast, 'input>,
    pub equal: tok::Equal,
    pub expr: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> Item<'ast, 'input> {
    pub fn new(
        let_: tok::Let,
        name: tok::Ident<'input>,
        generics: Vec<tok::Ident<'input>>,
        colon: tok::Colon,
        typ: &'ast Type<'ast, 'input>,
        equal: tok::Equal,
        expr: &'ast Expr<'ast, 'input>,
    ) -> Self {
        Item {
            let_,
            name,
            generics,
            colon,
            typ,
            equal,
            expr,
        }
    }
}
