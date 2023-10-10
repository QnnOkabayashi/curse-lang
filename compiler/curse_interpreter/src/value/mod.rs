use curse_hir::hir::Arm;
use curse_interner::{Ident, InternedString};
use std::{fmt, rc::Rc};

use crate::{error::EvalError, evaluation::Bindings};

pub type ValueRef<'hir> = Rc<Value<'hir>>;

// Type representing a value in curse. Subject to change as we potentially come up with better
// representations of these values
#[derive(Clone)]
pub enum Value<'hir> {
    Integer(u32),
    // String(&'hir str),
    Bool(bool),
    Function(&'hir [Arm<'hir>], Bindings<'hir>),
    Record(OwnedMap<ValueRef<'hir>>),
    Choice {
        ty: Ident,
        variant: Ident,
        value: ValueRef<'hir>,
    },
    Builtin(Builtin<'hir>),
}

impl Value<'_> {
    pub fn is_null(&self) -> bool {
        match self {
            Value::Record(map) => map.entries.is_empty(),
            _ => false,
        }
    }
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Integer(int) => write!(f, "{int}"),
            // String(string) => write!(f, "{string}"),
            Bool(bool) => write!(f, "{bool}"),
            Function(..) => write!(f, "<function>"),
            Builtin(_) => write!(f, "<builtin>"),
            Record(map) => write!(f, "{map:#?}"),
            Choice { ty, variant, value } => {
                write!(f, "{ty}::{variant} {value:?}")
            }
        }
    }
}

impl Default for Value<'_> {
    fn default() -> Self {
        Self::Record(OwnedMap::default())
    }
}

pub type Builtin<'hir> = fn(ValueRef<'hir>, ValueRef<'hir>) -> Result<ValueRef<'hir>, EvalError>;

#[derive(Clone)]
pub struct OwnedMap<T> {
    pub entries: Vec<(InternedString, T)>,
}

impl<T: fmt::Debug> fmt::Debug for OwnedMap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.entries.iter().map(|(name, value)| (name, value)))
            .finish()
    }
}

impl<'hir, T> OwnedMap<T> {
    pub fn new(entries: Vec<(InternedString, T)>) -> Self {
        OwnedMap { entries }
    }
}

impl<T> Default for OwnedMap<T> {
    fn default() -> Self {
        OwnedMap::new(vec![])
    }
}
