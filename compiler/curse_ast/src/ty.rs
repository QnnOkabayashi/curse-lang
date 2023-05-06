use crate::{pat, tok, ParseError, Res, Span};
use displaydoc::Display;

pub type TypeTuple<'ast, 'input> = pat::PatTuple<&'ast Type<'ast, 'input>>;

#[derive(Clone, Debug, Display)]
pub enum Type<'ast, 'input> {
    #[displaydoc("{0}")]
    Named(tok::NamedType<'input>),
    #[displaydoc("{0}")]
    Tuple(TypeTuple<'ast, 'input>),
    #[displaydoc("{0}")]
    Function(TypeFunction<'ast, 'input>),
}

impl<'ast, 'input> Type<'ast, 'input> {
    pub fn named_from_grammar(named_type: tok::NamedType<'input>) -> Self {
        Type::Named(named_type)
    }

    pub fn tuple_from_grammar(tuple: pat::PatTuple<Res<&'ast Type<'ast, 'input>>>) -> Res<Self> {
        Ok(Type::Tuple(TypeTuple {
            lparen: tuple.lparen,
            kind: tuple.kind.map(pat::TupleNonempty::transpose).transpose()?,
            rparen: tuple.rparen,
        }))
    }

    pub fn function_from_grammar(
        res_lhs: Res<&'ast Type<'ast, 'input>>,
        res_rhs: Res<&'ast Type<'ast, 'input>>,
        arrow: tok::Arrow,
        res_output: Res<&'ast Type<'ast, 'input>>,
    ) -> Res<Self> {
        match (res_lhs, res_rhs, res_output) {
            (Ok(lhs), Ok(rhs), Ok(output)) => Ok(Type::Function(TypeFunction {
                lhs,
                rhs,
                arrow,
                output,
            })),
            _ => Err(ParseError),
        }
    }
}

impl Span for Type<'_, '_> {
    fn span(&self) -> (usize, usize) {
        match self {
            Type::Named(named) => named.span(),
            Type::Tuple(tuple) => tuple.span(),
            Type::Function(fun) => fun.span(),
        }
    }
}

#[derive(Clone, Debug, Display)]
#[displaydoc("{lhs}, {rhs} -> {output}")]
pub struct TypeFunction<'ast, 'input> {
    pub lhs: &'ast Type<'ast, 'input>,
    pub rhs: &'ast Type<'ast, 'input>,
    pub arrow: tok::Arrow,
    pub output: &'ast Type<'ast, 'input>,
}

impl Span for TypeFunction<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.lhs.span_between(self.output)
    }
}
