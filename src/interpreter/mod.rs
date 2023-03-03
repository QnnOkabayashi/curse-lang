use crate::ast::{Expr, Lit};
use crate::interpreter::{error::EvalError, value::Value};
use builtins::call_function;
use std::collections::HashMap;

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
        Expr::Tuple(elements) => elements
            .iter()
            .map(|it| eval(it, env))
            .collect::<Result<_, _>>()
            .map(Value::Tuple),
        Expr::Symbol(symbol) => Ok(Value::Symbol(*symbol)),
        Expr::Closure(f) => Ok(Value::Closure(f)),
        Expr::Appl(appl) => {
            let left = eval(appl.left, env)?;
            let right = eval(appl.right, env)?;
            let function = eval(appl.function, env)?;
            call_function(left, function, right, env)
        }
    }
}
