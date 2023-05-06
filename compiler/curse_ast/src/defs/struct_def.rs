use super::FieldKind;
use crate::{tok, Span};

/// A definition for a new struct type.
///
/// Example: `struct Account { balance: I32, online: Bool }`
#[derive(Clone, Debug)]
pub struct StructDef<'ast, 'input> {
    pub struct_: tok::Struct,
    pub name: tok::NamedType<'input>,
    pub generics: Vec<tok::NamedType<'input>>,
    pub fields: FieldKind<'ast, 'input>,
}

impl<'ast, 'input> StructDef<'ast, 'input> {
    pub fn from_grammar(
        struct_: tok::Struct,
        name: tok::NamedType<'input>,
        generics: Vec<tok::NamedType<'input>>,
        opt_fields: Option<FieldKind<'ast, 'input>>,
    ) -> Option<Self> {
        Some(StructDef {
            struct_,
            name,
            generics,
            fields: opt_fields?,
        })
    }
}

impl Span for StructDef<'_, '_> {
    fn span(&self) -> (usize, usize) {
        match self.fields {
            FieldKind::Newtype(ty) => self.struct_.span_between(ty),
            FieldKind::Record { rbrace, .. } => self.struct_.span_between(rbrace),
        }
    }
}
