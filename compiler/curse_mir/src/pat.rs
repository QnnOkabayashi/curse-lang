use crate::{Spanned, Ty, Type, TypeChoice, TypeKind, ValueIdent};

pub type Pat<'cx> = Spanned<PatKind<'cx>>;

#[derive(Copy, Clone, Debug)]
pub enum PatKind<'cx> {
    Bool(bool),
    I32(i32),
    Ident {
        ty: TypeKind<'cx>,
        literal: ValueIdent,
    },
    Tuple {
        ty: &'cx [Type<'cx>],
        pats: &'cx [Pat<'cx>],
    },
    Choice {
        ty: &'cx TypeChoice<'cx>,
        variant: u32,
        payload: Option<&'cx Pat<'cx>>,
    },
}

impl<'cx> PatKind<'cx> {
    pub fn unit() -> Self {
        PatKind::Tuple { ty: &[], pats: &[] }
    }
}

impl Default for PatKind<'_> {
    fn default() -> Self {
        PatKind::unit()
    }
}

impl<'cx> Ty<'cx> for Pat<'cx> {
    fn ty(&self) -> Type<'cx> {
        let kind = match self.kind {
            PatKind::Bool(_) => TypeKind::Bool,
            PatKind::I32(_) => TypeKind::I32,
            PatKind::Ident { ty, .. } => ty,
            PatKind::Tuple { ty, .. } => TypeKind::Record(ty),
            PatKind::Choice { ty, .. } => TypeKind::Choice(ty),
        };

        Type {
            kind,
            span: self.span,
        }
    }
}
