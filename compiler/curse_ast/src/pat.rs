use crate::{tok, ExprLit, Span};

mod tuple;
pub use tuple::*;

#[derive(Clone, Debug)]
pub enum Pat<'ast, 'input> {
    Lit(ExprLit<'input>),
    Choice(PatChoice<'ast, 'input>),
    Tuple(PatTuple<&'ast Pat<'ast, 'input>>),
}

#[derive(Clone, Debug)]
pub struct PatChoice<'ast, 'input> {
    pub apostrophe: tok::Apostrophe,
    pub tag: tok::Ident<'input>,
    pub payload: Option<&'ast Pat<'ast, 'input>>,
}

// impl<Lit> Span for PatChoice<'_, '_, Lit> {
//     fn span(&self) -> (usize, usize) {
//         if let Some(value) = self.payload {
//             self.apostrophe.span_between(value)
//         } else {
//             self.apostrophe.span_between(self.tag)
//         }
//     }
// }
