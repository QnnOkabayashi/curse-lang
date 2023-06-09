use crate::{tok, Span, Type};

#[derive(Clone, Debug)]
pub struct NamedType<'ast, 'input> {
    pub name: tok::TypeIdent<'input>,
    pub generic_args: Option<GenericArgs<'ast, 'input>>,
}

#[derive(Clone, Debug)]
pub enum GenericArgs<'ast, 'input> {
    /// Example: `Vec I32`
    Single(&'ast Type<'ast, 'input>),
    /// Example: `Result (I32 * Error)`
    CartesianProduct {
        lparen: tok::LParen,
        types: Vec<(Type<'ast, 'input>, tok::Star)>,
        trailing: &'ast Type<'ast, 'input>,
        rparen: tok::RParen,
    },
}

impl Span for NamedType<'_, '_> {
    fn start(&self) -> usize {
        self.name.start()
    }

    fn end(&self) -> usize {
        if let Some(generic_args) = self.generic_args.as_ref() {
            generic_args.end()
        } else {
            self.name.end()
        }
    }
}

impl Span for GenericArgs<'_, '_> {
    fn start(&self) -> usize {
        match self {
            GenericArgs::Single(ty) => ty.start(),
            GenericArgs::CartesianProduct { lparen, .. } => lparen.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            GenericArgs::Single(ty) => ty.end(),
            GenericArgs::CartesianProduct { rparen, .. } => rparen.end(),
        }
    }

    fn span(&self) -> (usize, usize) {
        match self {
            GenericArgs::Single(ty) => ty.span(),
            GenericArgs::CartesianProduct { lparen, rparen, .. } => (lparen.start(), rparen.end()),
        }
    }
}
