use crate::{tok, Record, Span};

mod named;
pub use named::{GenericArgs, NamedType};

pub type RecordType<'ast, 'input> = Record<FieldType<'ast, 'input>>;

#[derive(Clone, Debug)]
pub enum Type<'ast, 'input> {
    Named(NamedType<'ast, 'input>),
    Record(RecordType<'ast, 'input>),
    Error,
}

#[derive(Clone, Debug)]
pub struct FieldType<'ast, 'input> {
    pub name: tok::Ident<'input>,
    pub colon: tok::Colon,
    pub ty: &'ast Type<'ast, 'input>,
}

impl Span for Type<'_, '_> {
    fn start(&self) -> usize {
        match self {
            Type::Named(named) => named.start(),
            Type::Record(record) => record.start(),
            Type::Error => todo!(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Type::Named(named) => named.end(),
            Type::Record(record) => record.end(),
            Type::Error => todo!(),
        }
    }

    fn span(&self) -> (usize, usize) {
        match self {
            Type::Named(named) => named.span(),
            Type::Record(record) => record.span(),
            Type::Error => todo!(),
        }
    }
}
