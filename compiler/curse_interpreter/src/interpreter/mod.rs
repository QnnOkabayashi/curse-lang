use crate::interpreter::{
    error::EvalError,
    // pattern_matching::{check_args, match_args},
    value::Value,
};

use curse_ast::ExprSymbol;
use curse_hir::{Builtin, Expr};
use std::collections::HashMap;

mod error;
mod value;

pub type Environment<'hir, 'input> = HashMap<&'input str, Value<'hir, 'input>>;

pub fn eval_expr<'input, 'hir>(
    env: &mut Environment<'hir, 'input>,
    expr: &'hir Expr<'hir, 'input>,
) -> Result<Value<'hir, 'input>, EvalError<'input>> {
    match expr.kind {
        // TODO(william) make this not horrific
        curse_hir::ExprKind::Builtin(b) => match b {
            Builtin::Add => Ok(Value::Symbol(ExprSymbol::Plus(curse_ast::tok::Plus {
                location: expr.span.0,
            }))),
            Builtin::Sub => Ok(Value::Symbol(ExprSymbol::Minus(curse_ast::tok::Minus {
                location: expr.span.0,
            }))),
            Builtin::Mul => Ok(Value::Symbol(ExprSymbol::Star(curse_ast::tok::Star {
                location: expr.span.0,
            }))),
            Builtin::Rem => Ok(Value::Symbol(ExprSymbol::Percent(
                curse_ast::tok::Percent {
                    location: expr.span.0,
                },
            ))),
            Builtin::Div => Ok(Value::Symbol(ExprSymbol::Slash(curse_ast::tok::Slash {
                location: expr.span.0,
            }))),
            Builtin::Eq => Ok(Value::Symbol(ExprSymbol::Eq(curse_ast::tok::Eq {
                location: expr.span.0,
            }))),
            Builtin::Lt => Ok(Value::Symbol(ExprSymbol::Lt(curse_ast::tok::Lt {
                location: expr.span.0,
            }))),
            Builtin::Gt => Ok(Value::Symbol(ExprSymbol::Gt(curse_ast::tok::Gt {
                location: expr.span.0,
            }))),
            Builtin::Le => Ok(Value::Symbol(ExprSymbol::Le(curse_ast::tok::Le {
                location: expr.span.0,
            }))),
            Builtin::Ge => Ok(Value::Symbol(ExprSymbol::Ge(curse_ast::tok::Ge {
                location: expr.span.0,
            }))),
        },
        curse_hir::ExprKind::I32(n) => Ok(Value::Integer(n)),
        curse_hir::ExprKind::Bool(b) => Ok(Value::Boolean(b)),
        curse_hir::ExprKind::Ident { ty: _, literal } => env
            .get(literal)
            .ok_or(EvalError::UnboundVariable {
                var: literal,
                span: expr.span,
            })
            .cloned(),
        curse_hir::ExprKind::Tuple { ty: _, exprs } => exprs
            .iter()
            .map(|e| eval_expr(env, e))
            .collect::<Result<Vec<Value>, EvalError>>()
            .map(Value::Tuple),
        curse_hir::ExprKind::Closure { ty: _, arms } => Ok(Value::Closure(arms)),
        curse_hir::ExprKind::Appl { ty: _, appl } => {
            let lhs = eval_expr(env, &appl.lhs)?;
            let rhs = eval_expr(env, &appl.rhs)?;
            let function = eval_expr(env, &appl.function)?;
            apply_function(env, lhs, rhs, function)
        }
    }
}

fn apply_function<'hir, 'input>(
    env: &mut Environment<'hir, 'input>,
    lhs: Value<'hir, 'input>,
    rhs: Value<'hir, 'input>,
    function: Value<'hir, 'input>,
) -> Result<Value<'hir, 'input>, EvalError<'input>> {
    match function {
        Value::Builtin(builtin) => builtin(env, lhs, rhs),
        Value::Closure(arms) => todo!(),
        Value::Symbol(op) => symbol(env, lhs, rhs, op),
        _ => unreachable!("has been type checked, so shouldn't be possible?"),
    }
}

// pub mod builtins;
// mod error;
// mod pattern_matching;
// mod value;
// use builtins::default_env;
//
//
// pub fn function_definition<'ast, 'input>(
//     name: &'input str,
//     expr: &'ast Expr<'ast, 'input>,
//     env: &mut Environment<'ast, 'input>,
// ) -> Result<(), EvalError<'input>> {
//     env.insert(name, eval_expr(expr, env)?);
//     Ok(())
// }
//
// pub fn eval_program<'input>(program: Program<'_, 'input>) -> Result<(), EvalError<'input>> {
//     let mut env = default_env();
//
//     // eval top level statements
//     for stmt in &program.items {
//         match stmt {
//             Item::Function(function) => {
//                 function_definition(function.name.literal, &function.closure, &mut env);
//             }
//         };
//     }
//
//     // find and execute `main`
//     if let Some(Value::Closure(closure)) = env.get("main") {
//         match closure {
//             ExprClosure {
//                 head:
//                     ExprBranch {
//                         params: ExprParams::Zero,
//                         body,
//                         ..
//                     },
//                 tail,
//             } if tail.is_empty() => {
//                 let mut new_env = env.clone();
//                 eval_expr(body, &mut new_env)?;
//             }
//             _ => return Err(EvalError::TypeMismatch),
//         }
//     }
//     Ok(())
// }
//
// pub fn eval_expr<'ast, 'input>(
//     expr: &'ast Expr<'ast, 'input>,
//     env: &mut Environment<'ast, 'input>,
// ) -> Result<Value<'ast, 'input>, EvalError<'input>> {
//     match expr {
//         Expr::Paren(ExprParen { inner, .. }) => eval_expr(inner, env),
//         Expr::Lit(ExprLit::Integer(token)) => token
//             .literal
//             .parse()
//             .map(Value::Integer)
//             .map_err(|_| EvalError::ParseInt(token.span().into())),
//         Expr::Lit(ExprLit::True(_)) => Ok(Value::Boolean(true)),
//         Expr::Lit(ExprLit::False(_)) => Ok(Value::Boolean(false)),
//         Expr::Lit(ExprLit::Ident(ident)) => env
//             .get(ident.literal)
//             .ok_or(EvalError::UnboundVariable {
//                 var: ident.literal,
//                 span: ident.span(),
//             })
//             .cloned(),
//         Expr::Tuple(tuple) => tuple
//             .iter_elements()
//             .map(|it| eval_expr(it, env))
//             .collect::<Result<_, _>>()
//             .map(Value::Tuple),
//         Expr::Symbol(symbol) => Ok(Value::Symbol(*symbol)),
//         Expr::Closure(closure) => Ok(Value::Closure(closure)),
//         Expr::Appl(appl) => {
//             let left = eval_expr(appl.lhs, env)?;
//             let right = eval_expr(appl.rhs, env)?;
//             let function = eval_expr(appl.function, env)?;
//             call_function(left, function, right, env)
//         }
//     }
// }
//
fn symbol<'hir, 'input>(
    env: &mut Environment<'hir, 'input>,
    lhs: Value<'hir, 'input>,
    rhs: Value<'hir, 'input>,
    op: ExprSymbol,
) -> Result<Value<'hir, 'input>, EvalError<'input>> {
    match op {
        ExprSymbol::Semi(_) => return Ok(rhs),
        ExprSymbol::Dot(_) => return apply_function(env, lhs, Value::default(), rhs),
        _ => {}
    }

    match (lhs, rhs) {
        (Value::Integer(x), Value::Integer(y)) => match op {
            ExprSymbol::Plus(_) => Ok(Value::Integer(x + y)),
            ExprSymbol::Minus(_) => Ok(Value::Integer(x - y)),
            ExprSymbol::Star(_) => Ok(Value::Integer(x * y)),
            ExprSymbol::Percent(_) => Ok(Value::Integer(x % y)),
            ExprSymbol::Slash(_) => Ok(Value::Integer(x / y)),
            ExprSymbol::DotDot(_) => Ok(Value::Vector((x..y).map(Value::Integer).collect())),
            ExprSymbol::Semi(_) => Ok(Value::Integer(y)),
            ExprSymbol::Eq(_) => Ok(Value::Boolean(x == y)),
            ExprSymbol::Lt(_) => Ok(Value::Boolean(x < y)),
            ExprSymbol::Gt(_) => Ok(Value::Boolean(x > y)),
            ExprSymbol::Le(_) => Ok(Value::Boolean(x <= y)),
            ExprSymbol::Ge(_) => Ok(Value::Boolean(x >= y)),
            _ => unreachable!("Dot handled above"),
        },
        _ => unreachable!("Should be type checked?"),
    }
}
//
// pub fn call_function<'ast, 'input>(
//     lhs: Value<'ast, 'input>,
//     function: Value<'ast, 'input>,
//     rhs: Value<'ast, 'input>,
//     env: &mut Environment<'ast, 'input>,
// ) -> Result<Value<'ast, 'input>, EvalError<'input>> {
//     match function {
//         Value::Integer(_) | Value::Tuple(_) | Value::Vector(_) | Value::Boolean(_) => {
//             Err(EvalError::TypeMismatch)
//         }
//         Value::Symbol(s) => symbol(lhs, rhs, s, env),
//         Value::Builtin(f) => f(lhs, rhs, env),
//         Value::Closure(closure) => {
//             let mut new_env = env.clone();
//             for branch in closure.iter_branches() {
//                 if check_args(&lhs, &rhs, &branch.params).is_ok() {
//                     match_args(lhs, rhs, &branch.params, &mut new_env)?;
//                     return eval_expr(branch.body, &mut new_env);
//                 }
//             }
//             Err(EvalError::FailedPatternMatch)
//         }
//     }
// }
