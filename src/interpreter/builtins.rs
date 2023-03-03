use super::eval;
use crate::ast::{Ident, Params, Pat, Symbol};
use crate::interpreter::{error::EvalError, value::Value};
use std::collections::HashMap;
use std::iter;

pub fn default_env<'ast, 'input>() -> HashMap<&'input str, Value<'ast, 'input>> {
    let mut env = HashMap::new();

    env.insert("print", Value::Builtin(print));
    env.insert("in", Value::Builtin(inn));
    env.insert("map", Value::Builtin(map));
    env.insert("for_each", Value::Builtin(for_each));
    env.insert("reduce", Value::Builtin(reduce));

    env
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
        Pat::Ident(ident) => {
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
        Value::Closure(closure) => {
            let mut new_env = env.clone();
            add_params_to_env(lhs, rhs, &closure.params, &mut new_env)?;
            eval(closure.body, &mut new_env)
        }
    }
}

fn print<'ast, 'input>(
    left: Value<'ast, 'input>,
    _right: Value<'ast, 'input>,
    _env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    println!("{left}");
    Ok(Value::default())
}

fn inn<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    call_function(left, right, Value::default(), env)
}

fn map<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    let Value::Vector(vec) = left else {
        return Err(EvalError::TypeMismatch);
    };

    vec.into_iter()
        .map(|val| call_function(val, right.clone(), Value::default(), env))
        .collect::<Result<_, _>>()
        .map(Value::Vector)
}

fn for_each<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    let Value::Vector(vec) = left else {
        return Err(EvalError::TypeMismatch);
    };

    for val in vec.into_iter() {
        call_function(val, right.clone(), Value::default(), env)?;
    }

    Ok(Value::default())
}

fn reduce<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    let Value::Vector(vec) = left else {
        return Err(EvalError::TypeMismatch);
    };

    let mut vec = vec.into_iter();

    let Some(mut ret) = vec.next() else {
        return Ok(Value::default());
    };

    // it really sucks that I can't quite use `fold` or `reduce` here
    while let Some(val) = vec.next() {
        ret = call_function(ret, right.clone(), val, env)?;
    }
    Ok(ret)
}
