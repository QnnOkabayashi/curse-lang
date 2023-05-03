use std::fmt;

use displaydoc::Display;
use petgraph::graph::NodeIndex;
use thiserror::Error;

use crate::Hir;

pub enum Typevar<'hir> {
    /// An unbound type variable
    Unbound,
    // A bound type variable.
    Bound {
        ty: Type<'hir>,
        source: NodeIndex,
    },
}

impl<'hir> Typevar<'hir> {
    pub fn binding(&self) -> Option<&Type<'hir>> {
        match self {
            Typevar::Bound { ty, .. } => Some(ty),
            Typevar::Unbound => None,
        }
    }
}

#[derive(Copy, Clone, Display, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("T{0}")]
pub struct Var(pub usize);

#[derive(Debug, Error)]
#[error("Unbound typevar")]
pub struct UnboundTypevar;

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("{kind}")]
pub struct Type<'hir> {
    pub kind: TypeKind<'hir>,
    pub span: (usize, usize),
}

impl<'hir> Type<'hir> {
    pub fn dummy() -> Self {
        Type {
            kind: TypeKind::unit(),
            span: (0, 0),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TypeKind<'hir> {
    I32,
    Bool,
    Var(Var),
    Tuple(&'hir [Type<'hir>]),
    Function(&'hir TypeFunction<'hir>),
}

impl<'hir> TypeKind<'hir> {
    pub fn unit() -> Self {
        TypeKind::Tuple(&[])
    }

    /// Returns a [`Display`](fmt::Display)able type that prints a [`Type`],
    /// except with all type variables fully expanded as much as possible.
    pub fn pretty(self, hir: &'hir Hir<'hir, 'hir>) -> TypePrinter<'hir> {
        TypePrinter { ty: self, hir }
    }

    pub fn resolve(&self, hir: &Hir<'hir, '_>) -> Result<Self, UnboundTypevar> {
        let TypeKind::Var(var) = self else {
            return Ok(*self);
        };

        hir[*var].binding().ok_or(UnboundTypevar)?.kind.resolve(hir)
    }
}

impl fmt::Display for TypeKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Tuple(types) => {
                f.write_str("(")?;
                if let Some((head, tail)) = types.split_first() {
                    write!(f, "{head}")?;
                    for ty in tail {
                        write!(f, ", {ty}")?;
                    }
                }
                f.write_str(")")
            }
            TypeKind::Var(var) => write!(f, "{var}"),
            TypeKind::Function(fun) => write!(f, "{fun}"),
        }
    }
}

pub struct TypePrinter<'a> {
    ty: TypeKind<'a>,
    hir: &'a Hir<'a, 'a>,
}

impl fmt::Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Tuple(tuple) => {
                write!(f, "(")?;
                let mut iter = tuple.iter();
                if let Some(ty) = iter.next() {
                    write!(f, "{}", ty.kind.pretty(self.hir))?;
                }
                for ty in iter {
                    write!(f, ", {}", ty.kind.pretty(self.hir))?;
                }
                write!(f, ")")
            }
            TypeKind::Var(var) => {
                if let Some(ty) = self.hir[var].binding() {
                    write!(f, "{}", ty.kind.pretty(self.hir))
                } else {
                    write!(f, "{var}")
                }
            }
            TypeKind::Function(fun) => {
                write!(
                    f,
                    "({} {} -> {})",
                    fun.lhs.kind.pretty(self.hir),
                    fun.rhs.kind.pretty(self.hir),
                    fun.output.kind.pretty(self.hir)
                )
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("({lhs} {rhs} -> {output})")]
pub struct TypeFunction<'hir> {
    pub lhs: Type<'hir>,
    pub rhs: Type<'hir>,
    pub output: Type<'hir>,
}
