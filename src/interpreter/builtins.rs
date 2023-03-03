use super::eval;
use crate::ast::{self, Params, Pat, Symbol};
use crate::interpreter::{error::EvalError, value::Value};
use std::collections::HashMap;

pub fn default_env<'ast, 'input>() -> HashMap<&'input str, Value<'ast, 'input>> {
    let mut env = HashMap::new();

    env.insert("print", Value::Builtin(print));
    env.insert("in", Value::Builtin(inn));

    env
}

fn symbol<'ast, 'input>(
    left: &Value,
    right: &Value,
    op: Symbol,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match (left, right) {
        (Value::Integer(x), Value::Integer(y)) => match op {
            Symbol::Unit => Err(EvalError::TypeMismatch),
            Symbol::Plus => Ok(Value::Integer(x + y)),
            Symbol::Minus => Ok(Value::Integer(x - y)),
            Symbol::Times => Ok(Value::Integer(x * y)),
            Symbol::DotDot => todo!(),
            Symbol::Semi => todo!(),
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
        Value::Integer(_) | Value::Tuple(_) => Err(EvalError::TypeMismatch),
        Value::Symbol(s) => symbol(&left, &right, s),
        Value::Builtin(f) => f(left, right, env),
        Value::Closure(ast::Closure { params, body }) => match params {
            Params::Two(Pat::Item(x), Pat::Item(y)) => {
                let mut new_env = env.clone();
                new_env.insert(x.inner, left);
                new_env.insert(y.inner, right);
                let result = eval(body, &mut new_env)?;
                Ok(result)
            }
            Params::One(Pat::Item(x)) => {
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
    Ok(Value::Tuple(vec![]))
}

fn inn<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    call_function(left, right, Value::Tuple(vec![]), env)
}
