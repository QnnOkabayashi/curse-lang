use crate::{tok, ExprClosure, Res, Span, Type};

/// AST representation of a function definition.
#[derive(Clone, Debug)]
pub struct FnDef<'ast, 'input> {
    pub fn_: tok::Fn,
    pub name: tok::Ident<'input>,
    pub type_sig: Option<TypeSig<'ast, 'input>>,
    pub function: ExprClosure<'ast, 'input>,
}

impl<'ast, 'input> FnDef<'ast, 'input> {
    pub fn from_grammar(
        fn_: tok::Fn,
        name: tok::Ident<'input>,
        opt_res_type_sig: Option<Res<TypeSig<'ast, 'input>>>,
        res_function: Res<ExprClosure<'ast, 'input>>,
    ) -> Res<Self> {
        Ok(FnDef {
            fn_,
            name,
            type_sig: opt_res_type_sig.transpose()?,
            function: res_function?,
        })
    }
}

impl Span for FnDef<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.fn_.span_between(&self.function)
    }
}

#[derive(Clone, Debug)]
pub struct TypeSig<'ast, 'input> {
    pub generics: Vec<tok::NamedType<'input>>,
    pub colon: tok::Colon,
    pub ty: &'ast Type<'ast, 'input>,
    pub eq: tok::Eq,
}

impl<'ast, 'input> TypeSig<'ast, 'input> {
    pub fn from_grammar(
        generics: Vec<tok::NamedType<'input>>,
        colon: tok::Colon,
        ty: Res<&'ast Type<'ast, 'input>>,
        eq: tok::Eq,
    ) -> Res<Self> {
        ty.map(|ty| TypeSig {
            generics,
            colon,
            ty,
            eq,
        })
    }
}
