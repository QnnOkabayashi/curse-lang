use crate::Expr;

#[derive(Copy, Clone, Debug)]
pub enum ExprFields<'cx> {
    Newtype(&'cx Expr<'cx>),
    Record(&'cx [Expr<'cx>]),
}

