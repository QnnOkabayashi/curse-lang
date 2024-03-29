use crate::ast::Record;
use curse_span::{HasSpan, Span};

mod named;
pub use named::{GenericArgs, NamedType};

#[derive(Clone, Debug)]
pub enum Type {
    Named(Box<NamedType>),
    // Failing to specify the type of a field in a record should be reported during ast lowering,
    // not during parsing, so we allow for a type to be omitted in this representation.
    Record(Box<Record<Self>>),
    Error,
}

impl HasSpan for Type {
    fn start(&self) -> u32 {
        match self {
            Type::Named(named) => named.start(),
            Type::Record(record) => record.start(),
            Type::Error => todo!(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Type::Named(named) => named.end(),
            Type::Record(record) => record.end(),
            Type::Error => todo!(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Type::Named(named) => named.span(),
            Type::Record(record) => record.span(),
            Type::Error => todo!(),
        }
    }
}
