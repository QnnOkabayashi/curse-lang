use crate::{ctx, Spanned, TypeIdent};
use displaydoc::Display;
use smallvec::SmallVec;
use thiserror::Error;

mod printer;
use printer::TypePrinter;

#[derive(Clone, Debug)]
pub struct TypeTemplate<'cx> {
    pub typevars: SmallVec<[Var; 4]>,
    pub ty: Type<'cx>,
}

impl<'cx> TypeTemplate<'cx> {
    pub fn new(ty: Type<'cx>) -> Self {
        TypeTemplate {
            typevars: SmallVec::new(),
            ty,
        }
    }
}

#[derive(Copy, Clone)]
pub enum Typevar<'cx> {
    /// An unbound type variable
    Unbound,
    // A bound type variable.
    Bound {
        ty: Type<'cx>,
    },
}

impl<'cx> Typevar<'cx> {
    pub fn binding(&self) -> Option<&Type<'cx>> {
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

pub type Type<'cx> = Spanned<TypeKind<'cx>>;

#[derive(Copy, Clone, Debug)]
pub enum TypeKind<'cx> {
    I32,
    Bool,
    EmptyRecord,
    Var(Var),
    Record(&'cx [Type<'cx>]),
    Choice(&'cx TypeChoice<'cx>),
    Function(&'cx TypeFunction<'cx>),
}

impl<'cx> TypeKind<'cx> {
    pub fn unit() -> Self {
        TypeKind::Record(&[])
    }

    /// Returns a [`Display`](fmt::Display)able type that prints a [`Type`],
    /// except with all type variables fully expanded as much as possible.
    pub fn display<'a>(self, ctx: &'a ctx::Typeck<'cx>) -> TypePrinter<'a, 'cx> {
        TypePrinter { ty: self, ctx }
    }

    pub fn resolve(&self, ctx: &ctx::Typeck<'cx>) -> Result<Self, UnboundTypevar> {
        let TypeKind::Var(var) = self else {
            return Ok(*self);
        };

        ctx[*var].binding().ok_or(UnboundTypevar)?.kind.resolve(ctx)
    }
}

impl Default for TypeKind<'_> {
    fn default() -> Self {
        TypeKind::unit()
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TypeFunction<'cx> {
    pub lhs: Type<'cx>,
    pub rhs: Type<'cx>,
    pub output: Type<'cx>,
}

#[derive(Debug)]
pub struct TypeChoice<'cx> {
    pub name: TypeIdent,
    pub variants: Vec<ChoiceVariant<'cx>>,
}

#[derive(Debug)]
pub struct ChoiceVariant<'cx> {
    pub name: TypeIdent,
    pub payload: Type<'cx>,
}
