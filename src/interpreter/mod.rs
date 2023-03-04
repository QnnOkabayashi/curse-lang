use crate::{
    ast::{Closure, Expr, Ident, Lit, Params, Program, Symbol, TopLevel},
    interpreter::{
        error::EvalError,
        pattern_matching::{check_args, match_args},
        value::Value,
    },
};
use std::collections::HashMap;

use self::builtins::default_env;

pub mod builtins;
mod error;
mod pattern_matching;
mod value;

pub type Environment<'ast, 'input> = HashMap<&'input str, Value<'ast, 'input>>;

pub fn function_definition<'ast, 'input>(
    name: &Ident<'input>,
    body: &'ast Closure<'ast, 'input>,
    env: &mut Environment<'ast, 'input>,
) {
    env.insert(name.inner, Value::Closure(body));
}

pub fn eval_program<'ast, 'input>(program: Program<'ast, 'input>) -> Result<(), EvalError<'input>> {
    let mut env = default_env();

    // eval top level statements
    for stmt in &program.stmts {
        match stmt {
            TopLevel::Function(name, body) => {
                function_definition(name, body, &mut env);
            }
            TopLevel::Expr(expr) => {
                eval_expr(expr, &mut env)?;
            }
        };
    }

    // find and execute `main`
    for (name, val) in &env {
        match (*name, val) {
            ("main", Value::Closure(Closure { branches })) if branches.len() == 1 => {
                // argc and argv?
                let Params::Zero = branches[0].params else {
                    return Err(EvalError::TypeMismatch)
                };

                let mut new_env = env.clone();
                eval_expr(branches[0].body, &mut new_env)?;
            }
            _ => (),
        }
    }

    Ok(())
}

pub fn eval_expr<'ast, 'input>(
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
            .map(|it| eval_expr(it, env))
            .collect::<Result<_, _>>()
            .map(Value::Tuple),
        Expr::Symbol(symbol) => Ok(Value::Symbol(*symbol)),
        Expr::Closure(closure) => Ok(Value::Closure(closure)),
        Expr::Appl(appl) => {
            let left = eval_expr(appl.left, env)?;
            let right = eval_expr(appl.right, env)?;
            let function = eval_expr(appl.function, env)?;
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
            Symbol::Asterisk => Ok(Value::Integer(x * y)),
            Symbol::Percent => Ok(Value::Integer(x % y)),
            Symbol::Slash => Ok(Value::Integer(x / y)),
            Symbol::DotDot => Ok(Value::Vector((x..y).map(Value::Integer).collect())),
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
        Value::Closure(Closure { branches }) => {
            let mut new_env = env.clone();
            for branch in branches {
                if check_args(&lhs, &rhs, &branch.params).is_ok() {
                    match_args(lhs, rhs, &branch.params, &mut new_env)?;
                    return eval_expr(branch.body, &mut new_env);
                }
            }
            Err(EvalError::FailedPatternMatch)
        }
    }
}
