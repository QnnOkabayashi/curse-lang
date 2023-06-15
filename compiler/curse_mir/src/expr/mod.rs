use crate::{Spanned, Type, TypeKind, ValueIdent};

mod arm;
pub use arm::ExprArm;

mod builtin;
pub use builtin::Builtin;

mod constructor;
pub use constructor::ExprFields;

pub trait Ty<'cx> {
    fn ty(&self) -> Type<'cx>;
}

pub type Expr<'cx> = Spanned<ExprKind<'cx>>;

#[derive(Copy, Clone, Debug)]
pub enum ExprKind<'cx> {
    Builtin(Builtin),
    I32(i32),
    Bool(bool),
    Ident {
        ty: TypeKind<'cx>,
        literal: ValueIdent,
    },
    Record {
        ty: TypeKind<'cx>,
        exprs: &'cx [Expr<'cx>],
    },
    Constructor {
        ty: TypeKind<'cx>,
        fields: &'cx Expr<'cx>,
    },
    Closure {
        ty: TypeKind<'cx>,
        arms: &'cx [ExprArm<'cx>],
    },
    Appl {
        ty: TypeKind<'cx>,
        appl: &'cx ExprAppl<'cx>,
    },
}

impl Default for ExprKind<'_> {
    fn default() -> Self {
        ExprKind::Record {
            ty: TypeKind::Record(&[]),
            exprs: &[],
        }
    }
}

#[derive(Debug)]
pub struct ExprAppl<'cx> {
    pub lhs: Expr<'cx>,
    pub function: Expr<'cx>,
    pub rhs: Expr<'cx>,
}

impl<'cx> Ty<'cx> for Expr<'cx> {
    fn ty(&self) -> Type<'cx> {
        match self.kind {
            ExprKind::Builtin(builtin) => Type {
                kind: builtin.type_kind(),
                span: self.span,
            },
            ExprKind::I32(_) => Type {
                kind: TypeKind::I32,
                span: self.span,
            },
            ExprKind::Bool(_) => Type {
                kind: TypeKind::Bool,
                span: self.span,
            },
            ExprKind::Ident { ty, .. }
            | ExprKind::Record { ty, .. }
            | ExprKind::Constructor { ty, .. }
            | ExprKind::Closure { ty, .. }
            | ExprKind::Appl { ty, .. } => Type {
                kind: ty,
                span: self.span,
            },
        }
    }
}
