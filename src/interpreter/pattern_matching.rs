use crate::ast::*;
use crate::interpreter::{error::EvalError, value::Value, Environment};
use std::iter;

pub fn match_args<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    params: &ExprParams<'ast, 'input>,
    env: &mut Environment<'ast, 'input>,
) -> Result<(), EvalError<'input>> {
    match params {
        ExprParams::Zero => match (left, right) {
            (Value::Tuple(t1), Value::Tuple(t2)) if t1.is_empty() && t2.is_empty() => Ok(()),
            _ => Err(EvalError::TypeMismatch),
        },
        ExprParams::One(param) => match right {
            Value::Tuple(t) if t.is_empty() => match_pattern(left, param.pat, env),
            _ => Err(EvalError::TypeMismatch),
        },
        ExprParams::Two(param1, _, param2) => {
            match_pattern(left, param1.pat, env)?;
            match_pattern(right, param2.pat, env)
        }
    }
}

pub fn check_args<'ast, 'input>(
    left: &Value<'ast, 'input>,
    right: &Value<'ast, 'input>,
    params: &ExprParams,
) -> Result<(), EvalError<'input>> {
    match params {
        ExprParams::Zero => match (left, right) {
            (Value::Tuple(t1), Value::Tuple(t2)) if t1.is_empty() && t2.is_empty() => Ok(()),
            _ => Err(EvalError::TypeMismatch),
        },
        ExprParams::One(param) => match right {
            Value::Tuple(t) if t.is_empty() => check_pattern(left, param.pat),
            _ => Err(EvalError::TypeMismatch),
        },
        ExprParams::Two(param1, _, param2) => {
            check_pattern(left, param1.pat)?;
            check_pattern(right, param2.pat)
        }
    }
}

pub fn match_pattern<'ast, 'input>(
    value: Value<'ast, 'input>,
    pattern: &Pat<'ast, ExprLit<'input>>,
    env: &mut Environment<'ast, 'input>,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Lit(lit) => match (value, lit) {
            (Value::Integer(x), ExprLit::Integer(y))
                if x == y.literal.parse::<i32>().expect("parse int failed") =>
            {
                Ok(())
            }
            (val, ExprLit::Ident(ident)) => {
                env.insert(ident.literal, val);
                Ok(())
            }
            _ => Err(EvalError::FailedPatternMatch),
        },
        Pat::Tuple(tuple) => match value {
            Value::Tuple(vals) if vals.len() == tuple.len() => {
                iter::zip(vals, tuple.iter_elements())
                    .try_for_each(|(val, pat)| match_pattern(val, pat, env))
            }
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}

pub fn check_pattern<'input>(
    value: &Value<'_, 'input>,
    pattern: &ExprPat,
) -> Result<(), EvalError<'input>> {
    match pattern {
        ExprPat::Lit(lit) => match (value, lit) {
            (Value::Integer(x), ExprLit::Integer(y))
                if *x == y.literal.parse::<i32>().expect("parse into failed") =>
            {
                Ok(())
            }
            (_, ExprLit::Ident(_)) => Ok(()),
            _ => Err(EvalError::FailedPatternMatch),
        },
        ExprPat::Tuple(pats) => match value {
            Value::Tuple(vals) if vals.len() == pats.len() => iter::zip(vals, pats.iter_elements())
                .try_for_each(|(val, pat)| check_pattern(val, pat)),
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}
