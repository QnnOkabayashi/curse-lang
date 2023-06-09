use crate::{tok, Lit, Record, Span};

pub type RecordPat<'ast, 'input> = Record<FieldPat<'ast, 'input>>;

#[derive(Clone, Debug)]
pub enum Pat<'ast, 'input> {
    Lit(Lit<'input>),
    Record(RecordPat<'ast, 'input>),
    // TODO(quinn): add support for struct and choice patterns
}

#[derive(Clone, Debug)]
pub struct FieldPat<'ast, 'input> {
    pub name: tok::Ident<'input>,
    pub explicit_value: Option<(tok::Colon, &'ast Pat<'ast, 'input>)>,
}

impl Span for Pat<'_, '_> {
    fn start(&self) -> usize {
        match self {
            Pat::Lit(lit) => lit.start(),
            Pat::Record(record) => record.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Pat::Lit(lit) => lit.end(),
            Pat::Record(record) => record.end(),
        }
    }

    fn span(&self) -> (usize, usize) {
        match self {
            Pat::Lit(lit) => lit.span(),
            Pat::Record(record) => record.span(),
        }
    }
}
