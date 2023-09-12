use std::{collections::HashMap, rc::Rc};

use crate::builtins;
use crate::error::EvalError;
use crate::value::{OwnedMap, Value, ValueRef};
use curse_hir::hir::{self, ExprKind, ExprRef, Lit, PatKind, PatRef, Program};
use curse_interner::InternedString;

// globally available functions, both regular named functions as well as type constructors
pub struct GlobalBindings<'hir> {
    // name of function => the function as a curse `Value`
    functions: HashMap<InternedString, ValueRef<'hir>>,

    // name of variant => name of enum
    constructors: HashMap<InternedString, InternedString>,
}

impl<'hir> GlobalBindings<'hir> {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            constructors: HashMap::new(),
        }
    }
}

pub type Bindings<'hir> = HashMap<InternedString, ValueRef<'hir>>;

fn eval_expr<'hir>(
    expr: ExprRef<'hir>,
    global_state: &GlobalBindings<'hir>,
    local_state: &mut Bindings<'hir>,
) -> Result<ValueRef<'hir>, EvalError> {
    match expr.kind {
        ExprKind::Symbol(hir::Symbol::Plus) => Ok(Rc::new(Value::Builtin(builtins::add))),
        ExprKind::Symbol(hir::Symbol::Star) => Ok(Rc::new(Value::Builtin(builtins::mul))),
        ExprKind::Symbol(hir::Symbol::Minus) => Ok(Rc::new(Value::Builtin(builtins::sub))),
        ExprKind::Symbol(hir::Symbol::Slash) => Ok(Rc::new(Value::Builtin(builtins::div))),
        ExprKind::Symbol(hir::Symbol::Percent) => Ok(Rc::new(Value::Builtin(builtins::modulo))),
        ExprKind::Symbol(hir::Symbol::Eq) => Ok(Rc::new(Value::Builtin(builtins::eq))),
        ExprKind::Symbol(hir::Symbol::Lt) => Ok(Rc::new(Value::Builtin(builtins::lt))),
        ExprKind::Symbol(hir::Symbol::Gt) => Ok(Rc::new(Value::Builtin(builtins::gt))),
        ExprKind::Symbol(hir::Symbol::Le) => Ok(Rc::new(Value::Builtin(builtins::le))),
        ExprKind::Symbol(hir::Symbol::Ge) => Ok(Rc::new(Value::Builtin(builtins::ge))),
        ExprKind::Symbol(hir::Symbol::Semi) => Ok(Rc::new(Value::Builtin(builtins::semi))),
        ExprKind::Symbol(hir::Symbol::Dot) => unimplemented!(),
        ExprKind::Symbol(hir::Symbol::DotDot) => unimplemented!(),
        ExprKind::Lit(Lit::Integer(int)) => Ok(Rc::new(Value::Integer(int))),
        ExprKind::Lit(Lit::Ident(ident)) => local_state
            .get(&ident.symbol)
            .or(global_state.functions.get(&ident.symbol))
            .ok_or(EvalError::UnboundVariable {
                literal: ident.to_string(),
                span: ident.span,
            })
            .cloned(),
        ExprKind::Lit(Lit::Bool(bool)) => Ok(Rc::new(Value::Bool(bool))),
        ExprKind::Record(map) => Ok(Rc::new(Value::Record(OwnedMap::new(
            map.entries
                .iter()
                .map(|(ident, expr)| {
                    Ok((
                        *ident,
                        eval_expr(
                            expr.ok_or(EvalError::MissingField)?,
                            global_state,
                            local_state,
                        )?,
                    ))
                })
                .collect::<Result<_, _>>()?,
        )))),
        ExprKind::Constructor(constructor) => Ok(Rc::new(Value::Choice {
            tag: constructor.path,
            value: eval_expr(constructor.inner, global_state, local_state)?,
        })),
        ExprKind::Closure(arms) => Ok(Rc::new(Value::Function(arms, local_state.clone()))),
        ExprKind::Appl(appl) => {
            let lhs = eval_expr(appl.lhs(), global_state, local_state)?;
            let fun = eval_expr(appl.fun(), global_state, local_state)?;
            let rhs = eval_expr(appl.rhs(), global_state, local_state)?;
            call_function(lhs, fun, rhs, global_state)
        }
        ExprKind::Region(_) => todo!("Regions"),
        ExprKind::Error => todo!("error handling"),
    }
}

fn call_function<'hir>(
    left: ValueRef<'hir>,
    function: ValueRef<'hir>,
    right: ValueRef<'hir>,
    global_state: &GlobalBindings<'hir>,
) -> Result<ValueRef<'hir>, EvalError> {
    match function.as_ref() {
        Value::Function(arms, closure_env) => {
            let arm = arms
                .iter()
                .find(|&arm| match arm.params.len() {
                    0 => left.is_null() && right.is_null(),
                    1 => check_pattern(&left, arm.params[0].pat) && right.is_null(),
                    2 => {
                        check_pattern(&left, arm.params[0].pat)
                            && check_pattern(&right, arm.params[1].pat)
                    }
                    _ => false,
                })
                .ok_or(EvalError::PatternMatchRefuted)?;

            // there's definitely some room for neat optimizations here
            let mut new_scope = closure_env.clone();
            match arm.params.len() {
                0 => eval_expr(arm.body, global_state, &mut new_scope),
                1 => {
                    match_pattern(left, arm.params[0].pat, &mut new_scope)?;
                    eval_expr(arm.body, global_state, &mut new_scope)
                }
                2 => {
                    match_pattern(left, arm.params[0].pat, &mut new_scope)?;
                    match_pattern(right, arm.params[1].pat, &mut new_scope)?;
                    eval_expr(arm.body, global_state, &mut new_scope)
                }
                _ => unreachable!("checked above"),
            }
        }
        Value::Builtin(builtin) => builtin(left, right),
        _ => Err(EvalError::TypeMismatch),
    }
}

fn check_pattern<'hir>(value: &Value, pattern: PatRef<'hir>) -> bool {
    match (&pattern.kind, value) {
        (PatKind::Record(pattern_map), Value::Record(value_map)) => {
            if pattern_map.entries.len() != value_map.entries.len() {
                false
            } else {
                pattern_map.entries.iter().zip(&value_map.entries).all(
                    |((_, opt_pat), (_, val))| match opt_pat {
                        Some(pat) => check_pattern(val.as_ref(), pat),
                        None => true,
                    },
                )
            }
        }
        (
            PatKind::Constructor(pat_tag, pattern),
            Value::Choice {
                tag: value_tag,
                value,
            },
        ) => {
            if pat_tag == value_tag {
                check_pattern(value, pattern)
            } else {
                false
            }
        }
        (PatKind::Lit(Lit::Integer(n)), Value::Integer(m)) => n == m,
        (PatKind::Lit(Lit::Bool(b1)), Value::Bool(b2)) => b1 == b2,
        (PatKind::Lit(Lit::Ident(_)), _) => true,
        _ => false,
    }
}

fn match_pattern<'hir>(
    value: ValueRef<'hir>,
    pattern: PatRef<'hir>,
    local_state: &mut Bindings<'hir>,
) -> Result<(), EvalError> {
    if let PatKind::Lit(Lit::Ident(ident)) = pattern.kind {
        local_state.insert(ident.symbol, value);
        Ok(())
    } else {
        match (&pattern.kind, value.as_ref()) {
            (PatKind::Record(pattern_map), Value::Record(value_map)) => {
                if pattern_map.entries.len() == value_map.entries.len() {
                    // if there's no pattern after the name, binds value to the name, otherwise
                    // matches on the pattern
                    for ((name, opt_pat), (_, val)) in
                        pattern_map.entries.iter().zip(&value_map.entries)
                    {
                        if let Some(pat) = opt_pat {
                            match_pattern(Rc::clone(&val), pat, local_state)?;
                        } else {
                            local_state.insert(name.symbol, Rc::clone(&val));
                        }
                    }

                    // should be ok since we already made sure the pattern
                    // successfully matched
                    Ok(())
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
            (
                PatKind::Constructor(pat_tag, pattern),
                Value::Choice {
                    tag: value_tag,
                    value,
                },
            ) => {
                if pat_tag == value_tag {
                    match_pattern(value.clone(), pattern, local_state)
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
            (PatKind::Lit(Lit::Integer(n)), Value::Integer(m)) => (*n == *m)
                .then_some(())
                .ok_or(EvalError::FailedPatternMatch),
            (PatKind::Lit(Lit::Bool(b1)), Value::Bool(b2)) => (*b1 == *b2)
                .then_some(())
                .ok_or(EvalError::FailedPatternMatch),
            (PatKind::Lit(Lit::Ident(_)), _) => {
                unreachable!("handled above, dang Rc making pattern matching annoying")
            }
            _ => Err(EvalError::FailedPatternMatch),
        }
    }
}

pub fn execute_program<'hir>(program: &Program<'hir>) -> Result<ValueRef<'hir>, EvalError> {
    let mut global_state = GlobalBindings::new();

    for (name, def) in &program.function_defs {
        global_state
            .functions
            .insert(*name, Rc::new(Value::Function(def.arms, HashMap::new())));
    }

    for (name, def) in &program.choice_defs {
        for (variant, _) in def.variants.entries {
            global_state.constructors.insert(variant.symbol, *name);
        }
    }

    call_function(
        Rc::new(Value::default()),
        global_state
            .functions
            .get(&InternedString::get_or_intern("main"))
            .ok_or(EvalError::MissingMain)
            .cloned()?,
        Rc::new(Value::default()),
        &global_state,
    )
}
