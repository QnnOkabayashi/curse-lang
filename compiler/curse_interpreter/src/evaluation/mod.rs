use crate::builtins;
use crate::error::EvalError;
use crate::value::{OwnedMap, Value, ValueRef};
use curse_hir::hir::{self, Expr, ExprKind, Lit, Pat, PatKind, Program};
use curse_interner::InternedString;
use std::{collections::HashMap, rc::Rc};

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

fn eval_record<'hir>(
    fields: impl IntoIterator<Item = (hir::Pat<'hir>, Option<Result<Rc<Value<'hir>>, EvalError>>)>,
    global_state: &GlobalBindings<'hir>,
    local_state: &Bindings<'hir>,
) -> Result<OwnedMap<Rc<Value<'hir>>>, EvalError> {
    let mut entries = vec![];

    for (field_pat, opt_expr) in fields {
        let expr =
            opt_expr.unwrap_or_else(|| pat_as_expr(&field_pat, global_state, local_state))?;

        match_pattern(expr, &field_pat, &mut |ident, value| {
            entries.push((ident, value))
        })?;
    }

    Ok(OwnedMap::new(entries))
}

fn eval_expr<'hir>(
    expr: &Expr<'hir>,
    global_state: &GlobalBindings<'hir>,
    local_state: &Bindings<'hir>,
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
            .get(&ident)
            .or_else(|| global_state.functions.get(&ident))
            .ok_or(EvalError::UnboundVariable {
                literal: ident.to_string(),
                span: expr.span,
            })
            .cloned(),
        ExprKind::Lit(Lit::Bool(bool)) => Ok(Rc::new(Value::Bool(bool))),
        ExprKind::Record(entries) => eval_record(
            entries.iter().map(|(field_pat, opt_expr)| {
                let opt_value = opt_expr
                    .as_ref()
                    .map(|expr| eval_expr(expr, global_state, local_state));

                (*field_pat, opt_value)
            }),
            global_state,
            local_state,
        )
        .map(|fields| Rc::new(Value::Record(fields))),
        ExprKind::Constructor(constructor) => Ok(Rc::new(Value::Choice {
            ty: constructor.ty,
            variant: constructor.variant,
            value: eval_expr(constructor.kind, global_state, local_state)?,
        })),
        ExprKind::Closure(arms) => Ok(Rc::new(Value::Function(
            arms.as_slice(),
            local_state.clone(),
        ))),
        ExprKind::Appl(appl) => {
            let lhs = eval_expr(&appl.lhs, global_state, local_state)?;
            let fun = eval_expr(&appl.fun, global_state, local_state)?;
            let rhs = eval_expr(&appl.rhs, global_state, local_state)?;
            call_function(lhs, fun, rhs, global_state)
        }
        ExprKind::Region(_) => todo!("Regions"),
        ExprKind::Error => todo!("error handling"),
    }
}

fn pat_as_expr<'hir>(
    pat: &Pat<'hir>,
    global_state: &GlobalBindings<'hir>,
    local_state: &Bindings<'hir>,
) -> Result<Rc<Value<'hir>>, EvalError> {
    let value = match pat.kind {
        PatKind::Lit(Lit::Ident(ident)) => local_state
            .get(&ident)
            .or_else(|| global_state.functions.get(&ident))
            .cloned()
            .ok_or_else(|| EvalError::UnboundVariable {
                literal: format!("{pat:?}"),
                span: pat.span,
            })?,
        PatKind::Lit(Lit::Integer(num)) => Rc::new(Value::Integer(num)),
        PatKind::Lit(Lit::Bool(b)) => Rc::new(Value::Bool(b)),
        PatKind::Record(fields) => {
            // { { a: b }: { a: b } }
            // If we see a record expression like `{ { a, b } }`
            // then the `pat` would be `{ a, b }`.
            // This function will generate the `{ a, b }` expr
            // which is then used for matching to evaluate `{ { a, b }: { a, b } }`
            // which evaluates to just `{ a, b }`
            let fields = eval_record(
                fields.iter().map(|(field_pat, opt_pat)| {
                    let opt_value = opt_pat
                        .as_ref()
                        .map(|pat| pat_as_expr(pat, global_state, local_state));

                    (*field_pat, opt_value)
                }),
                global_state,
                local_state,
            )?;

            Rc::new(Value::Record(fields))
        }
        PatKind::Constructor(constructor) => Rc::new(Value::Choice {
            ty: constructor.ty,
            variant: constructor.variant,
            value: pat_as_expr(constructor.kind, global_state, local_state)?,
        }),
        PatKind::Error => todo!(),
    };

    Ok(value)
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
                    1 => check_pattern(&left, &arm.params[0].pat) && right.is_null(),
                    2 => {
                        check_pattern(&left, &arm.params[0].pat)
                            && check_pattern(&right, &arm.params[1].pat)
                    }
                    _ => false,
                })
                .ok_or(EvalError::PatternMatchRefuted)?;

            // there's definitely some room for neat optimizations here
            let mut new_scope = closure_env.clone();
            let mut push_binding_fn = |ident, value| {
                new_scope.insert(ident, value);
            };
            match arm.params.len() {
                0 => eval_expr(&arm.body, global_state, &mut new_scope),
                1 => {
                    match_pattern(left, &arm.params[0].pat, &mut push_binding_fn)?;
                    eval_expr(&arm.body, global_state, &mut new_scope)
                }
                2 => {
                    match_pattern(left, &arm.params[0].pat, &mut push_binding_fn)?;
                    match_pattern(right, &arm.params[1].pat, &mut push_binding_fn)?;
                    eval_expr(&arm.body, global_state, &mut new_scope)
                }
                _ => unreachable!("checked above"),
            }
        }
        Value::Builtin(builtin) => builtin(left, right),
        _ => Err(EvalError::TypeMismatch),
    }
}

fn check_pattern<'hir>(value: &Value, pattern: &Pat<'hir>) -> bool {
    match (&pattern.kind, value) {
        (PatKind::Record(pattern_map), Value::Record(value_map)) => {
            if pattern_map.len() != value_map.entries.len() {
                false
            } else {
                pattern_map
                    .iter()
                    .zip(&value_map.entries)
                    .all(|((_, opt_pat), (_, val))| match opt_pat {
                        Some(pat) => check_pattern(val.as_ref(), pat),
                        None => true,
                    })
            }
        }
        (
            PatKind::Constructor(hir::Constructor {
                ty: pat_ty,
                variant: pat_variant,
                kind,
            }),
            Value::Choice {
                ty: value_ty,
                variant: value_variant,
                value,
            },
        ) => {
            // `&&` short-circuits according to Rust reference.
            pat_ty == value_ty && pat_variant == value_variant && check_pattern(value, kind)
        }
        (PatKind::Lit(Lit::Integer(n)), Value::Integer(m)) => n == m,
        (PatKind::Lit(Lit::Bool(b1)), Value::Bool(b2)) => b1 == b2,
        (PatKind::Lit(Lit::Ident(_)), _) => true,
        _ => false,
    }
}

fn match_record<'hir>(
    bindings: &[(hir::Pat<'hir>, Option<Pat<'hir>>)],
    // When matching on another record, get_value looks up keys from the other record
    // When constructing, it pulls idents from the local/global bindings
    get_value: &mut dyn FnMut(InternedString) -> Option<Rc<Value<'hir>>>,
    push_binding: &mut dyn FnMut(InternedString, Rc<Value<'hir>>),
) -> Result<(), EvalError> {
    for (field_pat, opt_value_pat) in bindings {
        match field_pat.kind {
            PatKind::Lit(Lit::Ident(ident)) => {
                let value = get_value(ident).ok_or(EvalError::FailedPatternMatch)?;

                if let Some(value_pat) = opt_value_pat {
                    match_pattern(value, value_pat, push_binding)?;
                } else {
                    push_binding(ident, value);
                }
            }
            PatKind::Lit(_) => return Err(EvalError::FailedPatternMatch),
            PatKind::Record(fields) => {
                if let Some(pat) = opt_value_pat {
                    // e.g. `{ { a, b }: ab, c }: { a: 1, b: 2, c: 3 }`
                    // `new_entries` is us making the `{ a, b }` map in the binding,
                    // where we fill it with values from the parent according to its fields
                    // but push to a fresh record.
                    let mut new_entries = vec![];
                    match_record(&fields[..], get_value, &mut |ident, value| {
                        new_entries.push((ident, value));
                    })?;

                    // Now that we've created a fresh record, we take it and try to bind it
                    // against the pattern
                    match_pattern(
                        Rc::new(Value::Record(OwnedMap::new(new_entries))),
                        pat,
                        push_binding,
                    )?;
                } else {
                    // `{ { a, b } }: { a, b }`
                    // kinda pointless, just inline a and b directly into the parent
                    match_record(bindings, get_value, push_binding)?;
                }
            }
            PatKind::Constructor(_) => todo!(),
            PatKind::Error => todo!(),
        }
        // match field_pat {
        //     hir::Binding::Ident(field_ident) => {
        //         let Some(value) = value_fields.remove(&field_ident.symbol) else {
        //             return Err(EvalError::FailedPatternMatch);
        //         };
        //
        //         if let Some(pat) = opt_value_pat {
        //             // pattern `{ a: <PAT> }` will bind the value of field `a` to pattern <PAT>,
        //             // e.g. `{ a: foo }` will bind the value of field `a` to ident `foo`.
        //             match_pattern(value, &pat, push_binding)?;
        //         } else {
        //             // pattern `{ a }` will bind the value of field `a` to ident `a`
        //             push_binding(field_ident.symbol, value);
        //         }
        //     }
        //     hir::Binding::Record(fields) => {
        //         if let Some(pat) = opt_value_pat {
        //             // e.g. `{ { a, b }: ab, c }: { a: 1, b: 2, c: 3 }`
        //             // `new_entries` is us making the `{ a, b }` map in the binding,
        //             // where we fill it with values from the parent according to its fields
        //             // but push to a fresh record.
        //             let mut new_entries = vec![];
        //             match_record(&fields[..], value_fields, &mut |ident, value| {
        //                 new_entries.push((ident, value));
        //             })?;
        //
        //             // Now that we've created a fresh record, we take it and try to bind it
        //             // against the pattern
        //             match_pattern(
        //                 Rc::new(Value::Record(OwnedMap::new(new_entries))),
        //                 pat,
        //                 push_binding,
        //             )?;
        //         } else {
        //             // `{ { a, b } }: { a, b }`
        //             // kinda pointless, just inline a and b directly into the parent
        //             match_record(bindings, value_fields, push_binding)?;
        //         }
        //     }
        //     hir::Binding::Error => todo!(),
        // }
    }
    Ok(())
}

fn match_pattern<'hir>(
    value: ValueRef<'hir>,
    pattern: &Pat<'hir>,
    push_binding: &mut dyn FnMut(InternedString, Rc<Value<'hir>>),
) -> Result<(), EvalError> {
    if let PatKind::Lit(Lit::Ident(ident)) = pattern.kind {
        push_binding(ident, value);
        Ok(())
    } else {
        match (&pattern.kind, value.as_ref()) {
            (PatKind::Record(pattern_map), Value::Record(value_map)) => {
                let mut value_fields: HashMap<InternedString, Rc<Value>> = value_map
                    .entries
                    .iter()
                    .map(|(ident, value)| (*ident, Rc::clone(&value)))
                    .collect();

                match_record(
                    &pattern_map[..],
                    &mut |ident| value_fields.remove(&ident),
                    push_binding,
                )?;

                // Should have seen all the fields now (otherwise it's not matchable)
                if value_fields.is_empty() {
                    Ok(())
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
            (
                PatKind::Constructor(hir::Constructor {
                    ty: pat_ty,
                    variant: pat_variant,
                    kind,
                }),
                Value::Choice {
                    ty: value_ty,
                    variant: value_variant,
                    value,
                },
            ) => {
                if pat_ty == value_ty && pat_variant == value_variant {
                    match_pattern(value.clone(), kind, push_binding)
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
            (PatKind::Lit(Lit::Integer(a)), Value::Integer(b)) => {
                if *a == *b {
                    Ok(())
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
            (PatKind::Lit(Lit::Bool(a)), Value::Bool(b)) => {
                if *a == *b {
                    Ok(())
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
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
        global_state.functions.insert(
            *name,
            Rc::new(Value::Function(def.arms.as_slice(), HashMap::new())),
        );
    }

    for (name, def) in &program.choice_defs {
        for (variant, _) in def.variants.iter() {
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
