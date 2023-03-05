use super::error::EvalError;
use crate::ast::{Closure, Symbol};
use std::{collections::HashMap, fmt};

type BuiltinFn<'ast, 'input> = fn(
    Value<'ast, 'input>,
    Value<'ast, 'input>,
    &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>>;

#[derive(Clone)]
pub enum Value<'ast, 'input> {
    Integer(i32),
    Symbol(Symbol),
    Closure(&'ast Closure<'ast, 'input>),
    Tuple(Vec<Value<'ast, 'input>>),
    Vector(Vec<Value<'ast, 'input>>),
    Builtin(BuiltinFn<'ast, 'input>),
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
            Value::Symbol(Symbol::Unit(..)) => write!(f, "()"),
            Value::Symbol(Symbol::Star(_)) => write!(f, "*"),
            Value::Symbol(Symbol::Plus(_)) => write!(f, "+"),
            Value::Symbol(Symbol::Minus(_)) => write!(f, "-"),
            Value::Symbol(Symbol::Percent(_)) => write!(f, "%"),
            Value::Symbol(Symbol::Slash(_)) => write!(f, "/"),
            Value::Symbol(Symbol::DotDot(_)) => write!(f, ".."),
            Value::Symbol(Symbol::Semi(_)) => write!(f, ";"),
            Value::Tuple(t) => {
                write!(f, "(")?;
                if let Some((first, rest)) = t.split_first() {
                    write!(f, "{first}")?;
                    for val in rest {
                        write!(f, ", {val}")?;
                    }
                }
                write!(f, ")")
            }
            Value::Vector(v) => {
                write!(f, "[")?;
                if let Some((first, rest)) = v.split_first() {
                    write!(f, "{first}")?;
                    for val in rest {
                        write!(f, ", {val}")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}
