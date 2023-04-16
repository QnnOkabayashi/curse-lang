use crate::{pat, tok, Span};
use displaydoc::Display;

pub type TypeTuple<'ast, 'input> = pat::PatTuple<&'ast Type<'ast, 'input>>;

#[derive(Clone, Debug, Display)]
pub enum Type<'ast, 'input> {
    #[displaydoc("{0}")]
    Named(TypeNamed<'input>),
    #[displaydoc("{0}")]
    Tuple(TypeTuple<'ast, 'input>),
    #[displaydoc("{0}")]
    Function(TypeFunction<'ast, 'input>),
}

impl Span for Type<'_, '_> {
    fn span(&self) -> (usize, usize) {
        match self {
            Type::Named(named) => named.span(),
            Type::Tuple(tuple) => tuple.span(),
            Type::Function(fun) => fun.span(),
        }
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("{name}")]
pub struct TypeNamed<'input> {
    pub name: tok::Ident<'input>,
}

impl<'input> TypeNamed<'input> {
    pub fn new(name: tok::Ident<'input>) -> Self {
        TypeNamed { name }
    }
}

impl Span for TypeNamed<'_> {
    fn span(&self) -> (usize, usize) {
        self.name.span()
    }
}

#[derive(Clone, Debug, Display)]
#[displaydoc("{lhs}, {rhs} -> {ret}")]
pub struct TypeFunction<'ast, 'input> {
    pub lhs: &'ast Type<'ast, 'input>,
    pub rhs: &'ast Type<'ast, 'input>,
    pub arrow: tok::Arrow,
    pub ret: &'ast Type<'ast, 'input>,
}

impl<'ast, 'input> TypeFunction<'ast, 'input> {
    pub fn new(
        lhs: &'ast Type<'ast, 'input>,
        rhs: &'ast Type<'ast, 'input>,
        arrow: tok::Arrow,
        ret: &'ast Type<'ast, 'input>,
    ) -> Self {
        TypeFunction {
            lhs,
            rhs,
            arrow,
            ret,
        }
    }
}

impl Span for TypeFunction<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.lhs.span_between(self.ret)
    }
}


