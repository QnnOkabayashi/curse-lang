use super::error::EvalError;
use crate::ast::*;
use std::{collections::HashMap, fmt};

type BuiltinFn<'ast, 'input> = fn(
    Value<'ast, 'input>,
    Value<'ast, 'input>,
    &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>>;

#[derive(Clone)]
pub enum Value<'ast, 'input> {
    Integer(i32),
    Symbol(ExprSymbol),
    Boolean(bool),
    Closure(&'ast ExprClosure<'ast, 'input>),
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
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Closure(_) => write!(f, "<closure>"),
            Value::Builtin(_) => write!(f, "<builtin>"),
            Value::Symbol(ExprSymbol::Star(_)) => write!(f, "*"),
            Value::Symbol(ExprSymbol::Plus(_)) => write!(f, "+"),
            Value::Symbol(ExprSymbol::Minus(_)) => write!(f, "-"),
            Value::Symbol(ExprSymbol::Percent(_)) => write!(f, "%"),
            Value::Symbol(ExprSymbol::Slash(_)) => write!(f, "/"),
            Value::Symbol(ExprSymbol::DotDot(_)) => write!(f, ".."),
            Value::Symbol(ExprSymbol::Semi(_)) => write!(f, ";"),
            Value::Symbol(ExprSymbol::Equal(_)) => write!(f, "="),
            Value::Symbol(ExprSymbol::Less(_)) => write!(f, "<"),
            Value::Symbol(ExprSymbol::Greater(_)) => write!(f, ">"),
            Value::Symbol(ExprSymbol::LessEqual(_)) => write!(f, "<="),
            Value::Symbol(ExprSymbol::GreaterEqual(_)) => write!(f, ">="),
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
