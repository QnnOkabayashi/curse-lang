use curse_hir::hir::Arm;
use curse_interner::Ident;
use std::{fmt::Display, rc::Rc};

use crate::{error::EvalError, evaluation::Bindings};

pub type ValueRef<'hir> = Rc<Value<'hir>>;

// Type representing a value in curse. Subject to change as we potentially come up with better
// representations of these values
#[derive(Debug, Clone)]
pub enum Value<'hir> {
    Integer(u32),
    // String(&'hir str),
    Bool(bool),
    Function(&'hir [Arm<'hir>], Bindings<'hir>),
    Record(OwnedMap<ValueRef<'hir>>),
    Choice { tag: &'hir [Ident], value: ValueRef<'hir> },
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

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Integer(int) => write!(f, "{int}"),
            // String(string) => write!(f, "{string}"),
            Bool(bool) => write!(f, "{bool}"),
            Function(..) => write!(f, "<function>"),
            Builtin(_) => write!(f, "<builtin>"),
            Record(map) => write!(f, "{map:?}"),
            Choice { tag, value } => write!(f, "{tag:?} {value}"),
        }
    }
}

impl Default for Value<'_> {
    fn default() -> Self {
        Self::Record(OwnedMap::default())
    }
}

pub type Builtin<'hir> = fn(ValueRef<'hir>, ValueRef<'hir>) -> Result<ValueRef<'hir>, EvalError>;

#[derive(Clone, Debug)]
pub struct OwnedMap<T> {
    pub entries: Vec<(Ident, T)>,
}

impl<'hir, T> OwnedMap<T> {
    pub fn new(entries: Vec<(Ident, T)>) -> Self {
        OwnedMap { entries }
    }
}

impl<T> Default for OwnedMap<T> {
    fn default() -> Self {
        OwnedMap::new(vec![])
    }
}
