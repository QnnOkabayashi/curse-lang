use crate::{
    ast::{Expr, Ident, Lit, Params, Pat, Symbol},
    interpreter::{error::EvalError, value::Value},
};
use std::{collections::HashMap, iter};

pub mod builtins;
mod error;
mod value;

pub fn eval<'ast, 'input>(
    expr: &'ast Expr<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match expr {
        Expr::Lit(Lit::Integer(integer)) => Ok(Value::Integer(*integer)),
        Expr::Lit(Lit::Ident(ident)) => env
            .get(ident.inner)
            .ok_or(EvalError::UnboundVariable(ident.inner))
            .cloned(),
        Expr::Tuple(items) => items
            .iter()
            .map(|it| eval(it, env))
            .collect::<Result<_, _>>()
            .map(Value::Tuple),
        Expr::Symbol(symbol) => Ok(Value::Symbol(*symbol)),
        Expr::Closure(closure) => Ok(Value::Closure(closure)),
        Expr::Appl(appl) => {
            let left = eval(appl.left, env)?;
            let right = eval(appl.right, env)?;
            let function = eval(appl.function, env)?;
            call_function(left, function, right, env)
        }
    }
}

fn symbol<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    op: Symbol,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    if let Symbol::Semi = op {
        return Ok(right);
    }

    match (left, right) {
        (Value::Integer(x), Value::Integer(y)) => match op {
            Symbol::Unit => Err(EvalError::TypeMismatch),
            Symbol::Plus => Ok(Value::Integer(x + y)),
            Symbol::Minus => Ok(Value::Integer(x - y)),
            Symbol::Times => Ok(Value::Integer(x * y)),
            Symbol::DotDot => Ok(Value::Vector((x..y).map(|n| Value::Integer(n)).collect())),
            Symbol::Semi => Ok(Value::Integer(y)),
        },
        _ => Err(EvalError::TypeMismatch),
    }
}

fn add_params_to_env<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    params: &Params<Ident<'input>>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<(), EvalError<'input>> {
    match params {
        Params::Zero => match (left, right) {
            (Value::Tuple(t1), Value::Tuple(t2)) if t1.is_empty() && t2.is_empty() => Ok(()),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::One(pat) => match right {
            Value::Tuple(t) if t.is_empty() => add_param_to_env(left, &pat, env),
            _ => Err(EvalError::TypeMismatch),
        },
        Params::Two(pat1, pat2) => {
            add_param_to_env(left, &pat1, env)?;
            add_param_to_env(right, &pat2, env)
        }
    }
}

fn add_param_to_env<'ast, 'input>(
    arg: Value<'ast, 'input>,
    pattern: &Pat<Ident<'input>>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<(), EvalError<'input>> {
    match pattern {
        Pat::Item(ident) => {
            env.insert(ident.inner, arg);
            Ok(())
        }
        Pat::Tuple(pats) => match arg {
            Value::Tuple(vals) if vals.len() == pats.len() => {
                iter::zip(vals, pats).try_for_each(|(val, pat)| add_param_to_env(val, pat, env))
            }
            _ => Err(EvalError::FailedPatternMatch),
        },
    }
}

pub fn call_function<'ast, 'input>(
    lhs: Value<'ast, 'input>,
    function: Value<'ast, 'input>,
    rhs: Value<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match function {
        Value::Integer(_) | Value::Tuple(_) | Value::Vector(_) => Err(EvalError::TypeMismatch),
        Value::Symbol(s) => symbol(lhs, rhs, s),
        Value::Builtin(f) => f(lhs, rhs, env),
        Value::Closure(_closure) => {
            todo!()
            // let mut new_env = env.clone();
            // add_params_to_env(lhs, rhs, &closure.params, &mut new_env)?;
            // eval(closure.body, &mut new_env)
        }
    }
}
