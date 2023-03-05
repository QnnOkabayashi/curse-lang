use crate::ast::{Lit, Params, Pat};
use crate::interpreter::{error::EvalError, value::Value, Environment};
use std::iter;

pub fn match_args<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    params: &Params<'input>,
    env: &mut Environment<'ast, 'input>,
) -> Result<(), EvalError<'input>> {
    match params {
        Params::Zero => match (left, right) {
            (Value::Tuple(t1), Value::Tuple(t2)) if t1.is_empty() && t2.is_empty() => Ok(()),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::One(pat) => match right {
            Value::Tuple(t) if t.is_empty() => match_pattern(left, pat, env),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::Two(pat1, pat2) => {
            match_pattern(left, pat1, env)?;
            match_pattern(right, pat2, env)
        }
    }
}

pub fn check_args<'ast, 'input>(
    left: &Value<'ast, 'input>,
    right: &Value<'ast, 'input>,
    params: &Params,
) -> Result<(), EvalError<'input>> {
    match params {
        Params::Zero => match (left, right) {
            (Value::Tuple(t1), Value::Tuple(t2)) if t1.is_empty() && t2.is_empty() => Ok(()),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::One(pat) => match right {
            Value::Tuple(t) if t.is_empty() => check_pattern(left, pat),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::Two(pat1, pat2) => {
            check_pattern(left, pat1)?;
            check_pattern(right, pat2)
        }
    }
}

pub fn match_pattern<'ast, 'input>(
    value: Value<'ast, 'input>,
    pattern: &Pat<'input>,
    env: &mut Environment<'ast, 'input>,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Lit(lit) => match (value, lit) {
            (Value::Integer(x), Lit::Integer(y))
                if x == y.1.parse::<i32>().expect("parse int failed") =>
            {
                Ok(())
            }
            (val, Lit::Ident(ident)) => {
                env.insert(ident.1, val);
                Ok(())
            }
            _ => Err(EvalError::FailedPatternMatch),
        },
        Pat::Tuple(pats) => match value {
            Value::Tuple(vals) if vals.len() == pats.len() => iter::zip(vals, pats.iter_elements())
                .try_for_each(|(val, pat)| match_pattern(val, pat, env)),
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}

pub fn check_pattern<'input>(
    value: &Value<'_, 'input>,
    pattern: &Pat,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Lit(lit) => match (value, lit) {
            (Value::Integer(x), Lit::Integer(y))
                if *x == y.1.parse::<i32>().expect("parse into failed") =>
            {
                Ok(())
            }
            (_, Lit::Ident(_)) => Ok(()),
            _ => Err(EvalError::FailedPatternMatch),
        },
        Pat::Tuple(pats) => match value {
            Value::Tuple(vals) if vals.len() == pats.len() => iter::zip(vals, pats.iter_elements())
                .try_for_each(|(val, pat)| check_pattern(val, pat)),
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}
