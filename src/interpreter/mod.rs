use crate::ast::*;
use crate::interpreter::{
    error::EvalError,
    pattern_matching::{check_args, match_args},
    value::Value,
};
use std::collections::HashMap;

pub mod builtins;
mod error;
mod pattern_matching;
mod value;
use builtins::default_env;

pub type Environment<'ast, 'input> = HashMap<&'input str, Value<'ast, 'input>>;

pub fn function_definition<'ast, 'input>(
    name: &'input str,
    closure: &'ast ExprClosure<'ast, 'input>,
    env: &mut Environment<'ast, 'input>,
) {
    env.insert(name, Value::Closure(closure));
}

pub fn eval_program<'input>(program: Program<'_, 'input>) -> Result<(), EvalError<'input>> {
    let mut env = default_env();

    // eval top level statements
    for stmt in &program.items {
        match stmt {
            Item::Function(function) => {
                function_definition(function.name.literal, &function.closure, &mut env);
            }
        };
    }

    // find and execute `main`
    if let Some(Value::Closure(closure)) = env.get("main") {
        match closure {
            ExprClosure {
                branches,
                last:
                    ExprBranch {
                        params: ExprParams::Zero,
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
        Expr::Paren(ExprParen { inner, .. }) => eval_expr(inner, env),
        Expr::Lit(ExprLit::Integer(token)) => token
            .literal
            .parse()
            .map(Value::Integer)
            .map_err(|_| EvalError::ParseInt(token.span())),
        Expr::Lit(ExprLit::True(_)) => Ok(Value::Boolean(true)),
        Expr::Lit(ExprLit::False(_)) => Ok(Value::Boolean(false)),
        Expr::Lit(ExprLit::Ident(ident)) => env
            .get(ident.literal)
            .ok_or(EvalError::UnboundVariable(ident.literal))
            .cloned(),
        Expr::Tuple(tuple) => tuple
            .iter_elements()
            .map(|it| eval_expr(it, env))
            .collect::<Result<_, _>>()
            .map(Value::Tuple),
        Expr::Symbol(symbol) => Ok(Value::Symbol(*symbol)),
        Expr::Closure(closure) => Ok(Value::Closure(closure)),
        Expr::Appl(appl) => {
            let left = eval_expr(appl.lhs, env)?;
            let right = eval_expr(appl.rhs, env)?;
            let function = eval_expr(appl.function, env)?;
            call_function(left, function, right, env)
        }
        Expr::Error(_) => Err(EvalError::LexError),
    }
}

fn symbol<'ast, 'input>(
    left: Value<'ast, 'input>,
    right: Value<'ast, 'input>,
    op: ExprSymbol,
) -> Result<Value<'ast, 'input>, EvalError<'input>> {
    if let ExprSymbol::Semi(_) = op {
        return Ok(right);
    }

    match (left, right) {
        (Value::Integer(x), Value::Integer(y)) => match op {
            ExprSymbol::Plus(_) => Ok(Value::Integer(x + y)),
            ExprSymbol::Minus(_) => Ok(Value::Integer(x - y)),
            ExprSymbol::Star(_) => Ok(Value::Integer(x * y)),
            ExprSymbol::Percent(_) => Ok(Value::Integer(x % y)),
            ExprSymbol::Slash(_) => Ok(Value::Integer(x / y)),
            ExprSymbol::DotDot(_) => Ok(Value::Vector((x..y).map(Value::Integer).collect())),
            ExprSymbol::Semi(_) => Ok(Value::Integer(y)),
            ExprSymbol::Equal(_) => Ok(Value::Boolean(x == y)),
            ExprSymbol::Less(_) => Ok(Value::Boolean(x < y)),
            ExprSymbol::Greater(_) => Ok(Value::Boolean(x > y)),
            ExprSymbol::LessEqual(_) => Ok(Value::Boolean(x <= y)),
            ExprSymbol::GreaterEqual(_) => Ok(Value::Boolean(x >= y)),
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
        Value::Integer(_) | Value::Tuple(_) | Value::Vector(_) | Value::Boolean(_) => {
            Err(EvalError::TypeMismatch)
        }
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
