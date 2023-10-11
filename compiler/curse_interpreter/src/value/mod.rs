use curse_hir::hir::Arm;
use curse_interner::{Ident, InternedString};
use std::{fmt, rc::Rc};

use crate::{error::EvalError, evaluation::Bindings};

// Type representing a value in curse. Subject to change as we potentially come up with better
// representations of these values
#[derive(Clone)]
pub enum Value<'hir> {
    Integer(u32),
    // String(&'hir str),
    Bool(bool),
    Function(Rc<(&'hir [Arm<'hir>], Bindings<'hir>)>),
    Record(Rc<Vec<(InternedString, Value<'hir>)>>),
    Choice(Rc<(Ident, Ident, Value<'hir>)>),
    Builtin(Builtin<'hir>),
}

impl Value<'_> {
    pub fn is_null(&self) -> bool {
        match self {
            Value::Record(entries) => entries.is_empty(),
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
            Record(map) => f
                .debug_map()
                .entries(map.iter().map(|(a, b)| (a, b))) // I forgot why but YOU NEED THIS MAP
                .finish(),
            Choice(ty_variant_value) => {
                let (ty, variant, value) = ty_variant_value.as_ref();
                write!(f, "{ty}::{variant} {value:?}")
            }
        }
    }
}

impl Default for Value<'_> {
    fn default() -> Self {
        Self::Record(Rc::new(Vec::new()))
    }
}

pub type Builtin<'hir> = fn(Value<'hir>, Value<'hir>) -> Result<Value<'hir>, EvalError>;
