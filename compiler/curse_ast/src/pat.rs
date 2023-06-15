use crate::{tok, Lit, Record};
use curse_span::{HasSpan, Span};

#[derive(Clone, Debug)]
pub enum Pat<'ast, 'input> {
    Lit(Lit<'input>),
    Record(Record<'input, &'ast Pat<'ast, 'input>>),
    Struct(tok::TypeIdent<'input>, &'ast Pat<'ast, 'input>),
    // TODO(quinn): add support for struct and choice patterns
}

impl HasSpan for Pat<'_, '_> {
    fn start(&self) -> u32 {
        match self {
            Pat::Lit(lit) => lit.start(),
            Pat::Record(record) => record.start(),
            Pat::Struct(ident, _) => ident.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Pat::Lit(lit) => lit.end(),
            Pat::Record(record) => record.end(),
            Pat::Struct(_, inner) => inner.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Pat::Lit(lit) => lit.span(),
            Pat::Record(record) => record.span(),
            Pat::Struct(ident, inner) => Span {
                start: ident.start(),
                end: inner.end(),
            },
        }
    }
}
