use crate::Hir;
use curse_ast::tok;
use displaydoc::Display;
use petgraph::graph::NodeIndex;
use smallvec::SmallVec;
use std::fmt;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct TypeTemplate<'hir, 'input> {
    pub typevars: SmallVec<[Var; 4]>,
    pub ty: Type<'hir, 'input>,
}

impl<'hir, 'input> TypeTemplate<'hir, 'input> {
    pub fn new(ty: Type<'hir, 'input>) -> Self {
        TypeTemplate {
            typevars: SmallVec::new(),
            ty,
        }
    }
}

pub enum Typevar<'hir, 'input> {
    /// An unbound type variable
    Unbound,
    // A bound type variable.
    Bound {
        ty: Type<'hir, 'input>,
        source: NodeIndex,
    },
}

impl<'hir, 'input> Typevar<'hir, 'input> {
    pub fn binding(&self) -> Option<&Type<'hir, 'input>> {
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
pub struct Type<'hir, 'input> {
    pub kind: TypeKind<'hir, 'input>,
    pub span: (usize, usize),
}

impl<'hir, 'input> Type<'hir, 'input> {
    pub fn dummy() -> Self {
        Type {
            kind: TypeKind::unit(),
            span: (0, 0),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TypeKind<'hir, 'input> {
    I32,
    Bool,
    Var(Var),
    Tuple(&'hir [Type<'hir, 'input>]),
    Choice(&'hir TypeChoice<'hir, 'input>),
    Function(&'hir TypeFunction<'hir, 'input>),
}

impl<'hir, 'input> TypeKind<'hir, 'input> {
    pub fn unit() -> Self {
        TypeKind::Tuple(&[])
    }

    /// Returns a [`Display`](fmt::Display)able type that prints a [`Type`],
    /// except with all type variables fully expanded as much as possible.
    pub fn pretty(self, hir: &'hir Hir<'hir, 'hir>) -> TypePrinter<'hir> {
        TypePrinter { ty: self, hir }
    }

    pub fn resolve(&self, hir: &Hir<'hir, 'input>) -> Result<Self, UnboundTypevar> {
        let TypeKind::Var(var) = self else {
            return Ok(*self);
        };

        hir[*var].binding().ok_or(UnboundTypevar)?.kind.resolve(hir)
    }
}

impl fmt::Display for TypeKind<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Var(var) => write!(f, "{var}"),
            TypeKind::Tuple(tuple) => {
                write!(f, "(")?;
                let mut iter = tuple.iter();
                if let Some(ty) = iter.next() {
                    write!(f, "{}", ty.kind)?;
                }
                for ty in iter {
                    write!(f, ", {}", ty.kind)?;
                }
                write!(f, ")")
            }
            TypeKind::Choice(choice) => write!(f, "{}", choice.name),
            TypeKind::Function(fun) => {
                write!(
                    f,
                    "({} {} -> {})",
                    fun.lhs.kind, fun.rhs.kind, fun.output.kind,
                )
            }
        }
    }
}

pub struct TypePrinter<'a> {
    ty: TypeKind<'a, 'a>,
    hir: &'a Hir<'a, 'a>,
}

impl fmt::Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            TypeKind::Var(var) => {
                if let Some(ty) = self.hir[var].binding() {
                    write!(f, "{}", ty.kind.pretty(self.hir))
                } else {
                    write!(f, "{var}")
                }
            }
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
            TypeKind::Choice(choice) => write!(f, "{}", choice.name),
            TypeKind::Function(fun) => {
                write!(
                    f,
                    "({} {} -> {})",
                    fun.lhs.kind.pretty(self.hir),
                    fun.rhs.kind.pretty(self.hir),
                    fun.output.kind.pretty(self.hir)
                )
            }
            other => write!(f, "{other}"),
        }
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("({lhs} {rhs} -> {output})")]
pub struct TypeFunction<'hir, 'input> {
    pub lhs: Type<'hir, 'input>,
    pub rhs: Type<'hir, 'input>,
    pub output: Type<'hir, 'input>,
}

#[derive(Debug)]
pub struct TypeChoice<'hir, 'input> {
    pub name: tok::Ident<'input>,
    pub variants: Vec<ChoiceVariant<'hir, 'input>>,
}

#[derive(Debug)]
pub struct ChoiceVariant<'hir, 'input> {
    pub tag: tok::Ident<'input>,
    pub payload: Option<Type<'hir, 'input>>,
}
