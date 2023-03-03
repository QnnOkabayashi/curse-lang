use super::error::EvalError;
use crate::ast::{Closure, Symbol};
use std::{collections::HashMap, fmt};

#[derive(Clone)]
pub enum Value<'ast, 'input> {
    Integer(i32),
    Symbol(Symbol),
    Closure(&'ast Closure<'ast, 'input>),
    Tuple(Vec<Value<'ast, 'input>>),
    Vector(Vec<Value<'ast, 'input>>),
    Builtin(
        fn(
            Value<'ast, 'input>,
            Value<'ast, 'input>,
            &mut HashMap<&'input str, Value<'ast, 'input>>,
        ) -> Result<Value<'ast, 'input>, EvalError<'input>>,
    ),
}

impl<'ast, 'input> Default for Value<'ast, 'input> {
    fn default() -> Self {
        Value::Tuple(vec![])
    }
}

impl<'ast, 'input> fmt::Display for Value<'ast, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Closure(_) => write!(f, "<closure>"),
            Value::Builtin(_) => write!(f, "<builtin>"),
            Value::Symbol(Symbol::Unit) => write!(f, "()"),
            Value::Symbol(Symbol::Times) => write!(f, "*"),
            Value::Symbol(Symbol::Plus) => write!(f, "+"),
            Value::Symbol(Symbol::Minus) => write!(f, "-"),
            Value::Symbol(Symbol::DotDot) => write!(f, ".."),
            Value::Symbol(Symbol::Semi) => write!(f, ";"),
            Value::Tuple(t) => {
                write!(f, "(")?;
                let mut t = t.iter().peekable();
                while let Some(val) = t.next() {
                    write!(f, "{val}")?;
                    if t.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Value::Vector(v) => {
                write!(f, "[")?;
                let mut t = v.iter().peekable();
                while let Some(val) = t.next() {
                    write!(f, "{val}")?;
                    if t.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}
