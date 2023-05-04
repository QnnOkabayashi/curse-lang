use crate::{tok, ExprClosure, Span, Type};

/// AST representation of a function definition.
#[derive(Clone, Debug)]
pub struct FnDef<'ast, 'input> {
    pub fn_: tok::Fn,
    pub name: tok::Ident<'input>,
    pub type_sig: Option<TypeSig<'ast, 'input>>,
    pub function: ExprClosure<'ast, 'input>,
}

#[derive(Clone, Debug)]
pub struct TypeSig<'ast, 'input> {
    pub generics: Vec<tok::NamedType<'input>>,
    pub colon: tok::Colon,
    pub ty: &'ast Type<'ast, 'input>,
    pub eq: tok::Eq,
}

impl<'ast, 'input> FnDef<'ast, 'input> {
    pub fn new(
        fn_: tok::Fn,
        name: tok::Ident<'input>,
        type_sig: Option<TypeSig<'ast, 'input>>,
        function: ExprClosure<'ast, 'input>,
    ) -> Self {
        FnDef {
            fn_,
            name,
            type_sig,
            function,
        }
    }
}

impl Span for FnDef<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.fn_.span_between(&self.function)
    }
}

impl<'ast, 'input> TypeSig<'ast, 'input> {
    pub fn new(
        generics: Vec<tok::NamedType<'input>>,
        colon: tok::Colon,
        ty: &'ast Type<'ast, 'input>,
        eq: tok::Eq,
    ) -> Self {
        TypeSig {
            generics,
            colon,
            ty,
            eq,
        }
    }
}
