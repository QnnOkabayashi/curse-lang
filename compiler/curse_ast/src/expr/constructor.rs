use crate::{tok, Expr, Punct, Span};

#[derive(Clone, Debug)]
pub struct ExprConstructor<'ast, 'input> {
    pub name: tok::NamedType<'input>,
    pub fields: ExprFields<'ast, 'input>,
}

#[derive(Clone, Debug)]
pub enum ExprFields<'ast, 'input> {
    Newtype(&'ast Expr<'ast, 'input>),
    Record {
        lbrace: tok::LBrace,
        fields: Punct<ExprNamedField<'ast, 'input>, tok::Comma>,
        rbrace: tok::RBrace,
    },
}

impl<'ast, 'input> ExprFields<'ast, 'input> {
    pub fn newtype_from_grammar(value: &'ast Expr<'ast, 'input>) -> Self {
        ExprFields::Newtype(value)
    }

    pub fn record_from_grammar(
        lbrace: tok::LBrace,
        elements: Vec<(ExprNamedField<'ast, 'input>, tok::Comma)>,
        trailing: Option<ExprNamedField<'ast, 'input>>,
        rbrace: tok::RBrace,
    ) -> Self {
        ExprFields::Record {
            lbrace,
            fields: Punct { elements, trailing },
            rbrace,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprNamedField<'ast, 'input> {
    pub name: tok::Ident<'input>,
    pub colon: tok::Colon,
    pub value: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> ExprNamedField<'ast, 'input> {
    pub fn from_grammar(
        name: tok::Ident<'input>,
        colon: tok::Colon,
        value: &'ast Expr<'ast, 'input>,
    ) -> Self {
        ExprNamedField { name, colon, value }
    }
}

impl Span for ExprConstructor<'_, '_> {
    fn span(&self) -> (usize, usize) {
        todo!()
    }
}
