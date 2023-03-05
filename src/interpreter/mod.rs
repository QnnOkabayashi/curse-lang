
use crate::ast::{Branch, Closure, Expr, Lit, Params, Program, Symbol, TopLevel};
use crate::interpreter::{
    error::EvalError,
    pattern_matching::{check_args, match_args},
    value::Value,
};
use crate::lex::tok;
use std::collections::HashMap;

pub mod builtins;
mod error;
mod pattern_matching;
mod value;
use builtins::default_env;

pub type Environment<'ast, 'input> = HashMap<&'input str, Value<'ast, 'input>>;

pub fn function_definition<'ast, 'input>(
    name: &'input str,
    closure: &'ast Closure<'ast, 'input>,
    env: &mut Environment<'ast, 'input>,
) {
    env.insert(name, Value::Closure(closure));
}

pub fn eval_program<'input>(program: Program<'_, 'input>) -> Result<(), EvalError<'input>> {
    let mut env = default_env();

    // eval top level statements
    for stmt in &program.items {
        match stmt {
            TopLevel::Function(function) => {
                function_definition(function.name.1, &function.closure, &mut env);
            }
            TopLevel::Expr(expr) => {
                eval_expr(expr, &mut env)?;
            }
        };
    }

    // find and execute `main`
    if let Some(Value::Closure(closure)) = env.get("main") {
        match closure {
            Closure {
                branches,
                last:
                    Branch {
                        params: Params::Zero,
                        body,
                        ..
                    },
            } if branches.is_empty() => {
                let mut new_env = env.clone();
                eval_expr(body, &mut new_env)?;
            }
            _ => return Err(EvalError::TypeMismatch),
        }
    }
    Ok(())
}

pub fn eval_expr<'ast, 'input>(
    expr: &'ast Expr<'ast, 'input>,
    env: &mut Environment<'ast, 'input>,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    match expr {
        Expr::Lit(Lit::Integer(tok::Integer(span, slice))) => {
            let int = slice
                .parse()
                .map_err(|_| EvalError::ParseInt(span.clone()))?;
            Ok(Value::Integer(int))
        }
        Expr::Lit(Lit::Ident(ident)) => env
            .get(ident.1)
            .ok_or(EvalError::UnboundVariable(ident.1))
            .cloned(),
        Expr::Tuple(items) => items
            .iter_elements()
            .map(|it| eval_expr(it, env))
            .collect::<Result<_, _>>()
            .map(Value::Tuple),
        Expr::Symbol(Symbol::Unit(_, _)) => Ok(Value::default()),
        Expr::Symbol(symbol) => Ok(Value::Symbol(symbol.clone())),
        Expr::Closure(closure) => Ok(Value::Closure(closure)),
        Expr::Appl(appl) => {
            let left = eval_expr(appl.lhs, env)?;
            let right = eval_expr(appl.rhs, env)?;
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
    if let Symbol::Semi(_) = op {
        return Ok(right);
    }

    match (left, right) {
        (Value::Integer(x), Value::Integer(y)) => match op {
            Symbol::Unit(..) => Err(EvalError::TypeMismatch),
            Symbol::Plus(_) => Ok(Value::Integer(x + y)),
            Symbol::Minus(_) => Ok(Value::Integer(x - y)),
            Symbol::Star(_) => Ok(Value::Integer(x * y)),
            Symbol::Percent(_) => Ok(Value::Integer(x % y)),
            Symbol::Slash(_) => Ok(Value::Integer(x / y)),
            Symbol::DotDot(_) => Ok(Value::Vector((x..y).map(Value::Integer).collect())),
            Symbol::Semi(_) => Ok(Value::Integer(y)),
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
        Value::Closure(closure) => {
            let mut new_env = env.clone();
            for branch in closure.iter_branches() {
                if check_args(&lhs, &rhs, &branch.params).is_ok() {
                    match_args(lhs, rhs, &branch.params, &mut new_env)?;
                    return eval_expr(branch.body, &mut new_env);
                }
            }
            Err(EvalError::FailedPatternMatch)
        }
    }
}
