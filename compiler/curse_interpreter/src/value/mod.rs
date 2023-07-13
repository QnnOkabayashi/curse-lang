#![allow(dead_code)]

use curse_hir::hir::{Arm, Map};
use curse_interner::InternedString;
use std::{fmt::Display, rc::Rc};

// We'll probably want reference counting??? Maybe we can do more fancy garbage collection or
// borrow checking or something later, not sure
pub type ValueRef<'hir> = Rc<Value<'hir>>;

// Type representing a value in curse. Subject to change as we potentially come up with better
// representations of these values
#[derive(Debug, Clone)]
pub enum Value<'hir> {
    Integer(u32),
    String(&'hir str),
    Bool(bool),
    Function(&'hir [Arm<'hir>]),
    Record(Map<'hir, ValueRef<'hir>>),
    Choice {
        tag: InternedString,
        value: ValueRef<'hir>,
    },
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Integer(int) => write!(f, "{int}"),
            String(string) => write!(f, "{string}"),
            Bool(bool) => write!(f, "{bool}"),
            Function(_) => write!(f, "<function>"),
            Record(map) => write!(f, "{map:?}"),
            Choice { tag, value } => write!(f, "{tag} {value}"),
        }
    }
}

impl Default for Value<'_> {
    fn default() -> Self {
        Self::Record(Map { entries: &[] })
    }
}
