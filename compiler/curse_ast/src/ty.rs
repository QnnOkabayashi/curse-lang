use crate::{pat, tok};
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
