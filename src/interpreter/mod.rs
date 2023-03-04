use crate::{
    ast::{Closure, Expr, Lit, Symbol},
    interpreter::{
        error::EvalError,
        pattern_matching::{match_irrefutable_pattern, match_refutable_pattern},
        value::Value,
    },
};
use std::collections::HashMap;

use self::pattern_matching::{check_args, check_refutable_pattern, match_args};

pub mod builtins;
mod error;
mod pattern_matching;
mod value;

pub type Environment<'ast, 'input> = HashMap<&'input str, Value<'ast, 'input>>;

pub fn eval<'ast, 'input>(
    expr: &'ast Expr<'ast, 'input>,
    env: &mut Environment<'ast, 'input>,
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

pub fn call_function<'ast, 'input>(
    lhs: Value<'ast, 'input>,
    function: Value<'ast, 'input>,
    rhs: Value<'ast, 'input>,
    env: &mut Environment<'ast, 'input>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match function {
        Value::Integer(_) | Value::Tuple(_) | Value::Vector(_) => Err(EvalError::TypeMismatch),
        Value::Symbol(s) => symbol(lhs, rhs, s),
        Value::Builtin(f) => f(lhs, rhs, env),
        Value::Closure(Closure::Nonpiecewise(irrefutable_closure)) => {
            let mut new_env = env.clone();
            match_args(
                lhs,
                rhs,
                &irrefutable_closure.params,
                &mut new_env,
                match_irrefutable_pattern,
            )?;
            eval(irrefutable_closure.body, &mut new_env)
        }
        Value::Closure(Closure::Piecewise(branches)) => {
            let mut new_env = env.clone();
            for branch in branches {
                if check_args(&lhs, &rhs, &branch.params, check_refutable_pattern).is_ok() {
                    match_args(
                        lhs,
                        rhs,
                        &branch.params,
                        &mut new_env,
                        match_refutable_pattern,
                    )?;
                    return eval(branch.body, &mut new_env);
                }
            }
            Err(EvalError::FailedPatternMatch)
        }
    }
}
