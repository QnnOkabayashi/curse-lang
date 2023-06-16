use crate::{ast_struct, tok, Type};
use curse_span::{HasSpan, Span};

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct NamedType<'ast, 'input> {
        pub ident: tok::TypeIdent<'input>,
        pub generic_args: Option<GenericArgs<'ast, 'input>>,
    }
}

#[derive(Clone, Debug)]
pub enum GenericArgs<'ast, 'input> {
    /// Example: `Vec I32`
    Single(&'ast Type<'ast, 'input>),
    /// Example: `Result (I32 * Error)`
    CartesianProduct(
        tok::LParen,
        Vec<(Type<'ast, 'input>, tok::Star)>,
        &'ast Type<'ast, 'input>,
        tok::RParen,
    ),
}

impl HasSpan for NamedType<'_, '_> {
    fn start(&self) -> u32 {
        self.ident.start()
    }

    fn end(&self) -> u32 {
        if let Some(generic_args) = self.generic_args.as_ref() {
            generic_args.end()
        } else {
            self.ident.end()
        }
    }
}

impl HasSpan for GenericArgs<'_, '_> {
    fn start(&self) -> u32 {
        match self {
            GenericArgs::Single(ty) => ty.start(),
            GenericArgs::CartesianProduct(lparen, ..) => lparen.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            GenericArgs::Single(ty) => ty.end(),
            GenericArgs::CartesianProduct(.., rparen) => rparen.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            GenericArgs::Single(ty) => ty.span(),
            GenericArgs::CartesianProduct(lparen, .., rparen) => Span {
                start: lparen.start(),
                end: rparen.end(),
            },
        }
    }
}
