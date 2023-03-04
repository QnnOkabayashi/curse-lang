use crate::{
    ast::{Ident, Lit, Params, Pat},
    interpreter::{error::EvalError, value::Value, Environment},
};
use std::iter;

pub fn match_args<'ast, 'input, Item, F>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    params: &Params<Item>,
    env: &mut Environment<'ast, 'input>,
    matching_scheme: F,
) -> Result<(), EvalError<'input>>
where
    F: Fn(
        Value<'ast, 'input>,
        &Pat<Item>,
        &mut Environment<'ast, 'input>,
    ) -> Result<(), EvalError<'input>>,
{
    match params {
        Params::Zero => match (left, right) {
            (Value::Tuple(t1), Value::Tuple(t2)) if t1.is_empty() && t2.is_empty() => Ok(()),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::One(pat) => match right {
            Value::Tuple(t) if t.is_empty() => matching_scheme(left, pat, env),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::Two(pat1, pat2) => {
            matching_scheme(left, pat1, env)?;
            matching_scheme(right, pat2, env)
        }
    }
}

pub fn check_args<'ast, 'input, Item, F>(
    left: &Value<'ast, 'input>,
    right: &Value<'ast, 'input>,
    params: &Params<Item>,
    matching_scheme: F,
) -> Result<(), EvalError<'input>>
where
    F: Fn(
        &Value<'ast, 'input>,
        &Pat<Item>,
    ) -> Result<(), EvalError<'input>>,
{
    match params {
        Params::Zero => match (left, right) {
            (Value::Tuple(t1), Value::Tuple(t2)) if t1.is_empty() && t2.is_empty() => Ok(()),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::One(pat) => match right {
            Value::Tuple(t) if t.is_empty() => matching_scheme(left, pat),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::Two(pat1, pat2) => {
            matching_scheme(left, pat1)?;
            matching_scheme(right, pat2)
        }
    }
}

pub fn match_irrefutable_pattern<'ast, 'input>(
    value: Value<'ast, 'input>,
    pattern: &Pat<Ident<'input>>,
    env: &mut Environment<'ast, 'input>,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Item(ident) => {
            env.insert(ident.inner, value);
            Ok(())
        }
        Pat::Tuple(pats) => match value {
            Value::Tuple(vals) if vals.len() == pats.len() => iter::zip(vals, pats)
                .try_for_each(|(val, pat)| match_irrefutable_pattern(val, pat, env)),
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}

#[allow(dead_code)]
pub fn check_irrefutable_pattern<'input>(
    value: &Value<'_, 'input>,
    pattern: &Pat<Ident<'input>>,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Item(_) => Ok(()),
        Pat::Tuple(pats) => match value {
            Value::Tuple(vals) if vals.len() == pats.len() => {
                iter::zip(vals, pats).try_for_each(|(val, pat)| check_irrefutable_pattern(val, pat))
            }
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}

pub fn match_refutable_pattern<'ast, 'input>(
    value: Value<'ast, 'input>,
    pattern: &Pat<Lit<'input>>,
    env: &mut Environment<'ast, 'input>,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Item(ident) => match (value, ident) {
            (Value::Integer(x), Lit::Integer(y)) if x == *y => Ok(()),
            (val, Lit::Ident(ident)) => {
                env.insert(ident.inner, val);
                Ok(())
            }
            _ => Err(EvalError::FailedPatternMatch),
        },
        Pat::Tuple(pats) => match value {
            Value::Tuple(vals) if vals.len() == pats.len() => iter::zip(vals, pats)
                .try_for_each(|(val, pat)| match_refutable_pattern(val, pat, env)),
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}

pub fn check_refutable_pattern<'input>(
    value: &Value<'_, 'input>,
    pattern: &Pat<Lit<'input>>,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Item(ident) => match (value, ident) {
            (Value::Integer(x), Lit::Integer(y)) if *x == *y => Ok(()),
            (_, Lit::Ident(_)) => Ok(()),
            _ => Err(EvalError::FailedPatternMatch),
        },
        Pat::Tuple(pats) => match value {
            Value::Tuple(vals) if vals.len() == pats.len() => {
                iter::zip(vals, pats).try_for_each(|(val, pat)| check_refutable_pattern(val, pat))
            }
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}
