use crate::{tok, Expr, Span, Type};

/// AST representation of a function definition.
#[derive(Clone, Debug)]
pub struct FnDef<'ast, 'input> {
    pub let_: tok::Let,
    pub name: tok::Ident<'input>,
    pub generics: Vec<tok::Ident<'input>>,
    pub colon: tok::Colon,
    pub typ: &'ast Type<'ast, 'input>,
    pub equal: tok::Equal,
    pub expr: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> FnDef<'ast, 'input> {
    pub fn new(
        let_: tok::Let,
        name: tok::Ident<'input>,
        generics: Vec<tok::Ident<'input>>,
        colon: tok::Colon,
        typ: &'ast Type<'ast, 'input>,
        equal: tok::Equal,
        expr: &'ast Expr<'ast, 'input>,
    ) -> Self {
        FnDef {
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

impl Span for FnDef<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.let_.span_between(self.expr)
    }
}
