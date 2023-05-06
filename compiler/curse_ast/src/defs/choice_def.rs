use super::{inside_out, FieldKind, Punct};
use crate::{tok, Span};

/// A definition for a new choice type.
///
/// Example: `choice Option T { Some T, None () }`
#[derive(Clone, Debug)]
pub struct ChoiceDef<'ast, 'input> {
    pub choice: tok::Choice,
    pub name: tok::NamedType<'input>,
    pub generics: Vec<tok::NamedType<'input>>,
    pub lbrace: tok::LBrace,
    pub variant_defs: Punct<VariantDef<'ast, 'input>, tok::Comma>,
    pub rbrace: tok::RBrace,
}

impl<'ast, 'input> ChoiceDef<'ast, 'input> {
    pub fn from_grammar(
        choice: tok::Choice,
        name: tok::NamedType<'input>,
        generics: Vec<tok::NamedType<'input>>,
        lbrace: tok::LBrace,
        elements: Vec<(Option<VariantDef<'ast, 'input>>, tok::Comma)>,
        trailing: Option<Option<VariantDef<'ast, 'input>>>,
        rbrace: tok::RBrace,
    ) -> Option<Self> {
        let elements = elements
            .into_iter()
            .map(|(opt_field, comma)| opt_field.map(|field| (field, comma)))
            .collect::<Option<_>>()?;

        inside_out(trailing).map(|trailing| ChoiceDef {
            choice,
            name,
            generics,
            lbrace,
            variant_defs: Punct { elements, trailing },
            rbrace,
        })
    }
}

impl Span for ChoiceDef<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.choice.span_between(self.rbrace)
    }
}

/// A definition for a variant in a choice type.
///
/// Example: `Some T`
#[derive(Clone, Debug)]
pub struct VariantDef<'ast, 'input> {
    pub name: tok::NamedType<'input>,
    pub fields: FieldKind<'ast, 'input>,
}

impl<'ast, 'input> VariantDef<'ast, 'input> {
    pub fn from_grammar(
        name: tok::NamedType<'input>,
        opt_fields: Option<FieldKind<'ast, 'input>>,
    ) -> Option<Self> {
        Some(VariantDef {
            name,
            fields: opt_fields?,
        })
    }
}
