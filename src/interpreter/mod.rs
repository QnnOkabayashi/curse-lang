use crate::ast::{self, Expr, Lit, Pat};
use crate::interpreter::{error::EvalError, value::Value};
use builtins::call_function;
use std::collections::HashMap;

pub mod builtins;
mod error;
mod value;

fn eval_pat<'ast, 'input>(
    pat: &'ast Pat<Lit<'input>>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match pat {
        Pat::Item(Lit::Symbol(s)) => Ok(Value::Symbol(*s)),
        Pat::Item(Lit::Integer(n)) => Ok(Value::Integer(*n)),
        Pat::Item(Lit::Ident(ident)) => env
            .get(ident.inner)
            .ok_or(EvalError::UnboundVariable(ident.inner))
            .cloned(),
        Pat::Tuple(items) => Ok(Value::Tuple(
            items
                .iter()
                .map(|it| eval_pat(it, env))
                .collect::<Result<Vec<Value<'ast, 'input>>, EvalError<'input>>>()?,
        )),
    }
}

// this function is kinda nasty ngl
pub fn eval<'ast, 'input>(
    expr: &'ast Expr<'ast, 'input>,
    env: &mut HashMap<&'input str, Value<'ast, 'input>>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match expr {
        Expr::Pat(pat) => eval_pat(pat, env),
        Expr::Closure(f) => Ok(Value::Closure(f)),
        Expr::Appl(ast::Appl {
            left,
            function,
            right,
        }) => {
            let left = eval(left, env)?;
            let right = eval(right, env)?;
            let function = eval(function, env)?;
            call_function(left, function, right, env)
        }
    }
}
