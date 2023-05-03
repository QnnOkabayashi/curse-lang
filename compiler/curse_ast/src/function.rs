use crate::{tok, Expr, Span, Type};

/// AST representation of a function definition.
#[derive(Clone, Debug)]
pub struct FnDef<'ast, 'input> {
    pub fn_: tok::Fn,
    pub name: tok::Ident<'input>,
    pub generics: Vec<tok::Ident<'input>>,
    pub colon: tok::Colon,
    pub typ: &'ast Type<'ast, 'input>,
    pub eq: tok::Eq,
    pub expr: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> FnDef<'ast, 'input> {
    pub fn new(
        fn_: tok::Fn,
        name: tok::Ident<'input>,
        generics: Vec<tok::Ident<'input>>,
        colon: tok::Colon,
        typ: &'ast Type<'ast, 'input>,
        eq: tok::Eq,
        expr: &'ast Expr<'ast, 'input>,
    ) -> Self {
        FnDef {
            fn_,
            name,
            generics,
            colon,
            typ,
            eq,
            expr,
        }
    }
}

impl Span for FnDef<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.fn_.span_between(self.expr)
    }
}
