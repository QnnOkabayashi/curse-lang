use crate::interpreter::{call_function, value::Value, EvalError};
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

    for val in vec {
        ret = call_function(ret, right.clone(), val, env)?;
    }
    Ok(ret)
}
