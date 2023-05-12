use crate::{tok, Expr, Punct, Res, Span};

#[derive(Clone, Debug)]
pub struct ExprConstructor<'ast, 'input> {
    pub name: tok::NamedType<'input>,
    pub fields: ExprFields<'ast, 'input>,
}

impl<'ast, 'input> ExprConstructor<'ast, 'input> {
    pub fn newtype_from_grammar(
        name: tok::NamedType<'input>,
        value: Res<&'ast Expr<'ast, 'input>>,
    ) -> Res<Self> {
        value.map(|value| ExprConstructor {
            name,
            fields: ExprFields::Newtype(value),
        })
    }

    pub fn record_from_grammar(
        name: tok::NamedType<'input>,
        lbrace: tok::LBrace,
        elements: Vec<(Res<ExprNamedField<'ast, 'input>>, tok::Comma)>,
        trailing: Option<Res<ExprNamedField<'ast, 'input>>>,
        rbrace: tok::RBrace,
    ) -> Res<Self> {
        let elements = elements
            .into_iter()
            .map(|(res_field, comma)| res_field.map(|field| (field, comma)))
            .collect::<Res<_>>()?;

        trailing.transpose().map(|trailing| ExprConstructor {
            name,
            fields: ExprFields::Record {
                lbrace,
                fields: Punct { elements, trailing },
                rbrace,
            },
        })
    }
}

impl Span for ExprConstructor<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.name.span_between(&self.fields)
    }
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
    pub fn newtype_from_grammar(value: Res<&'ast Expr<'ast, 'input>>) -> Res<Self> {
        value.map(ExprFields::Newtype)
    }

    pub fn record_from_grammar(
        lbrace: tok::LBrace,
        elements: Vec<(Res<ExprNamedField<'ast, 'input>>, tok::Comma)>,
        trailing: Option<Res<ExprNamedField<'ast, 'input>>>,
        rbrace: tok::RBrace,
    ) -> Res<Self> {
        let elements = elements
            .into_iter()
            .map(|(res_field, comma)| res_field.map(|field| (field, comma)))
            .collect::<Res<_>>()?;

        trailing.transpose().map(|trailing| ExprFields::Record {
            lbrace,
            fields: Punct { elements, trailing },
            rbrace,
        })
    }
}

impl Span for ExprFields<'_, '_> {
    fn span(&self) -> (usize, usize) {
        match self {
            ExprFields::Newtype(ty) => ty.span(),
            ExprFields::Record { lbrace, rbrace, .. } => lbrace.span_between(rbrace),
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
        value: Res<&'ast Expr<'ast, 'input>>,
    ) -> Res<Self> {
        value.map(|value| ExprNamedField { name, colon, value })
    }
}
