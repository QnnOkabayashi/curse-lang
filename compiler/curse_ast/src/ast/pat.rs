use crate::ast::{Constructor, Lit, Record};
use curse_span::{HasSpan, Span};

#[derive(Clone, Debug)]
pub enum Pat {
    Lit(Lit),
    Record(Box<Record<Self>>),
    Constructor(Box<Constructor<Self>>),
}

impl HasSpan for Pat {
    fn start(&self) -> u32 {
        match self {
            Pat::Lit(lit) => lit.start(),
            Pat::Record(record) => record.start(),
            Pat::Constructor(constructor) => constructor.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Pat::Lit(lit) => lit.end(),
            Pat::Record(record) => record.end(),
            Pat::Constructor(constructor) => constructor.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Pat::Lit(lit) => lit.span(),
            Pat::Record(record) => record.span(),
            Pat::Constructor(constructor) => constructor.span(),
        }
    }
}
