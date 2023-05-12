use super::EvalError;
use curse_ast::*;
use curse_hir::{Expr, ExprArm};
use std::{collections::HashMap, fmt};

type BuiltinFn<'hir, 'input> = fn(
    &mut HashMap<&'input str, Value<'hir, 'input>>,
    Value<'hir, 'input>,
    Value<'hir, 'input>,
) -> Result<Value<'hir, 'input>, EvalError<'input>>;

#[derive(Clone)]
pub enum Value<'hir, 'input> {
    Integer(i32),
    Symbol(ExprSymbol),
    Boolean(bool),
    Closure(&'hir [ExprArm<'hir, 'input>]),
    Tuple(Vec<Value<'hir, 'input>>),
    Vector(Vec<Value<'hir, 'input>>),
    Builtin(BuiltinFn<'hir, 'input>),
}

impl<'hir, 'input> Default for Value<'hir, 'input> {
    fn default() -> Self {
        Value::Tuple(vec![])
    }
}

impl<'hir, 'input> fmt::Display for Value<'hir, 'input> {
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
            Value::Symbol(ExprSymbol::Dot(_)) => write!(f, "."),
            Value::Symbol(ExprSymbol::DotDot(_)) => write!(f, ".."),
            Value::Symbol(ExprSymbol::Semi(_)) => write!(f, ";"),
            Value::Symbol(ExprSymbol::Eq(_)) => write!(f, "="),
            Value::Symbol(ExprSymbol::Lt(_)) => write!(f, "<"),
            Value::Symbol(ExprSymbol::Gt(_)) => write!(f, ">"),
            Value::Symbol(ExprSymbol::Le(_)) => write!(f, "<="),
            Value::Symbol(ExprSymbol::Ge(_)) => write!(f, ">="),
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
