use crate::ast::pat;
use crate::lex::tok;
use displaydoc::Display;

#[derive(Clone, Debug, Display)]
pub enum Type<'ast, 'input> {
    #[displaydoc("{0}")]
    Named(Named<'input>),
    #[displaydoc("{0}")]
    Tuple(Tuple<'ast, 'input>),
    #[displaydoc("{0}")]
    Function(Function<'ast, 'input>),
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("{name}")]
pub struct Named<'input> {
    name: tok::Ident<'input>,
}

impl<'input> Named<'input> {
    pub fn new(name: tok::Ident<'input>) -> Self {
        Self { name }
    }
}

#[derive(Clone, Debug, Display)]
#[displaydoc("{tuple}")]
pub struct Tuple<'ast, 'input> {
    tuple: pat::Tuple<&'ast Type<'ast, 'input>>,
}

impl<'ast, 'input> Tuple<'ast, 'input> {
    pub fn new(tuple: pat::Tuple<&'ast Type<'ast, 'input>>) -> Self {
        Tuple { tuple }
    }
}

#[derive(Clone, Debug, Display)]
#[displaydoc("{lhs} {rhs} -> {ret}")]
pub struct Function<'ast, 'input> {
    lhs: &'ast Type<'ast, 'input>,
    rhs: &'ast Type<'ast, 'input>,
    arrow: tok::Arrow,
    ret: &'ast Type<'ast, 'input>,
}

impl<'ast, 'input> Function<'ast, 'input> {
    pub fn new(
        lhs: &'ast Type<'ast, 'input>,
        rhs: &'ast Type<'ast, 'input>,
        arrow: tok::Arrow,
        ret: &'ast Type<'ast, 'input>,
    ) -> Self {
        Self {
            lhs,
            rhs,
            arrow,
            ret,
        }
    }
}
