use super::eval;
use crate::ast::{self, Params, Pat, Symbol, Closure};
use crate::interpreter::{error::EvalError, value::Value};
use std::collections::HashMap;

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

pub fn call_function<'ast, 'input>(
    left: Value<'ast, 'input>,
    function: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match function {
        Value::Integer(_) | Value::Tuple(_) | Value::Vector(_) => Err(EvalError::TypeMismatch),
        Value::Symbol(s) => symbol(left, right, s),
        Value::Builtin(f) => f(left, right, env),
        Value::Closure(Closure { params, body }) => match params {
            Params::Two(Pat::Ident(x), Pat::Ident(y)) => {
                let mut new_env = env.clone();
                new_env.insert(x.inner, left);
                new_env.insert(y.inner, right);
                let result = eval(body, &mut new_env)?;
                Ok(result)
            }
            Params::One(Pat::Ident(x)) => {
                let mut new_env = env.clone();
                new_env.insert(x.inner, left);
                let result = eval(body, &mut new_env)?;
                Ok(result)
            }
            _ => todo!(),
        },
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

    Ok(Value::Vector(
        vec.into_iter()
            .map(|val| call_function(val, right.clone(), Value::default(), env))
            .collect::<Result<Vec<Value<'ast, 'input>>, EvalError<'input>>>()?,
    ))
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

    if vec.is_empty() {
        return Ok(Value::default());
    }

    // it really sucks that I can't quite use `fold` or `reduce` here
    let mut vec = vec.into_iter();
    let mut ret = vec.next().unwrap(); // guaranteed safe since vec isn't empty
    while let Some(val) = vec.next() {
        ret = call_function(ret, right.clone(), val, env)?;
    }
    Ok(ret)
}
