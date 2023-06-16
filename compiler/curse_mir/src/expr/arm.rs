use crate::{Expr, Pat};
use curse_ast::{tok, Span};

#[derive(Copy, Clone, Debug, Default)]
pub struct ExprArm<'cx> {
    pub open: tok::Pipe,
    pub lhs: Pat<'cx>,
    pub rhs: Pat<'cx>,
    pub close: tok::Pipe,
    pub body: Expr<'cx>,
}

impl Span for ExprArm<'_> {
    fn start(&self) -> usize {
        self.open.start()
    }

    fn end(&self) -> usize {
        self.body.end()
    }
}
