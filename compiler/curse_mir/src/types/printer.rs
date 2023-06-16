use crate::{ctx, TypeKind};
use std::fmt;

pub struct TypePrinter<'a, 'cx> {
    pub ty: TypeKind<'cx>,
    pub ctx: &'a ctx::Typeck<'cx>,
}

impl fmt::Display for TypePrinter<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            TypeKind::I32 => write!(f, "I32"),
            TypeKind::Bool => write!(f, "Bool"),
            TypeKind::Var(var) => {
                if let Some(ty) = self.ctx[var].binding() {
                    write!(f, "{}", ty.kind.display(self.ctx))
                } else {
                    write!(f, "{var}")
                }
            }
            TypeKind::Record(tuple) => {
                write!(f, "(")?;
                let mut iter = tuple.iter();
                if let Some(ty) = iter.next() {
                    write!(f, "{}", ty.kind.display(self.ctx))?;
                }
                for ty in iter {
                    write!(f, ", {}", ty.kind.display(self.ctx))?;
                }
                write!(f, ")")
            }
            TypeKind::Choice(choice) => {
                write!(f, "{}", choice.name.display(self.ctx.global))
            }
            TypeKind::Function(fun) => {
                write!(
                    f,
                    "({} {} -> {})",
                    fun.lhs.kind.display(self.ctx),
                    fun.rhs.kind.display(self.ctx),
                    fun.output.kind.display(self.ctx)
                )
            }
        }
    }
}
