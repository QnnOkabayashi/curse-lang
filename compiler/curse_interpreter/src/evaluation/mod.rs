use crate::builtins;
use crate::error::EvalError;
use crate::value::Value;
use curse_hir::hir::{self, Expr, ExprKind, Lit, Pat, PatKind, Program};
use curse_interner::InternedString;
use std::{collections::HashMap, rc::Rc};

// globally available functions, both regular named functions as well as type constructors
pub struct GlobalBindings<'hir> {
    // name of function => the function as a curse `Value`
    functions: HashMap<InternedString, Value<'hir>>,

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

pub type Bindings<'hir> = HashMap<InternedString, Value<'hir>>;

#[derive(Copy, Clone)]
struct BindingsInScope<'a, 'hir> {
    global_state: &'a GlobalBindings<'hir>,
    local_state: &'a Bindings<'hir>,
}

impl<'a, 'hir> BindingsInScope<'a, 'hir> {
    fn get(self, ident: &InternedString) -> Option<Value<'hir>> {
        self.local_state
            .get(ident)
            .or_else(|| self.global_state.functions.get(ident))
            .cloned()
    }
}

fn eval_record<'hir>(
    fields: impl IntoIterator<Item = (hir::Pat<'hir>, Option<Result<Value<'hir>, EvalError>>)>,
    bindings_in_scope: BindingsInScope<'_, 'hir>,
) -> Result<Vec<(InternedString, Value<'hir>)>, EvalError> {
    let mut entries = vec![];

    for (field_pat, opt_expr) in fields {
        let expr = opt_expr.unwrap_or_else(|| pat_as_expr(&field_pat, bindings_in_scope))?;

        match_pattern(expr, &field_pat, &mut |ident, value| {
            entries.push((ident, value))
        })?;
    }

    Ok(entries)
}

fn eval_expr<'hir>(
    expr: &Expr<'hir>,
    bindings_in_scope: BindingsInScope<'_, 'hir>,
) -> Result<Value<'hir>, EvalError> {
    match expr.kind {
        ExprKind::Symbol(hir::Symbol::Plus) => Ok(Value::Builtin(builtins::add)),
        ExprKind::Symbol(hir::Symbol::Star) => Ok(Value::Builtin(builtins::mul)),
        ExprKind::Symbol(hir::Symbol::Minus) => Ok(Value::Builtin(builtins::sub)),
        ExprKind::Symbol(hir::Symbol::Slash) => Ok(Value::Builtin(builtins::div)),
        ExprKind::Symbol(hir::Symbol::Percent) => Ok(Value::Builtin(builtins::modulo)),
        ExprKind::Symbol(hir::Symbol::Eq) => Ok(Value::Builtin(builtins::eq)),
        ExprKind::Symbol(hir::Symbol::Lt) => Ok(Value::Builtin(builtins::lt)),
        ExprKind::Symbol(hir::Symbol::Gt) => Ok(Value::Builtin(builtins::gt)),
        ExprKind::Symbol(hir::Symbol::Le) => Ok(Value::Builtin(builtins::le)),
        ExprKind::Symbol(hir::Symbol::Ge) => Ok(Value::Builtin(builtins::ge)),
        ExprKind::Symbol(hir::Symbol::Semi) => Ok(Value::Builtin(builtins::semi)),
        ExprKind::Symbol(hir::Symbol::Dot) => unimplemented!(),
        ExprKind::Symbol(hir::Symbol::DotDot) => unimplemented!(),
        ExprKind::Lit(Lit::Integer(int)) => Ok(Value::Integer(int)),
        ExprKind::Lit(Lit::Ident(ident)) => {
            bindings_in_scope
                .get(&ident)
                .ok_or_else(|| EvalError::UnboundVariable {
                    literal: ident.to_string(),
                    span: expr.span,
                })
        }
        ExprKind::Lit(Lit::Bool(bool)) => Ok(Value::Bool(bool)),
        ExprKind::Record(entries) => {
            let fields = eval_record(
                entries.iter().map(|(field_pat, opt_expr)| {
                    let opt_value = opt_expr
                        .as_ref()
                        .map(|expr| eval_expr(expr, bindings_in_scope));

                    (*field_pat, opt_value)
                }),
                bindings_in_scope,
            )?;

            Ok(Value::Record(Rc::new(fields)))
        }
        ExprKind::Constructor(constructor) => Ok(Value::Choice(
            constructor.ty,
            constructor.variant,
            Rc::new(eval_expr(constructor.kind, bindings_in_scope)?),
        )),
        ExprKind::Closure(arms) => Ok(Value::Function(
            arms.as_slice(),
            Rc::new(bindings_in_scope.local_state.clone()),
        )),
        ExprKind::Appl(appl) => {
            let lhs = eval_expr(&appl.lhs, bindings_in_scope)?;
            let fun = eval_expr(&appl.fun, bindings_in_scope)?;
            let rhs = eval_expr(&appl.rhs, bindings_in_scope)?;
            call_function(lhs, fun, rhs, bindings_in_scope.global_state)
        }
        ExprKind::Region(_) => todo!("Regions"),
        ExprKind::Error => todo!("error handling"),
    }
}

fn pat_as_expr<'hir>(
    pat: &Pat<'hir>,
    bindings_in_scope: BindingsInScope<'_, 'hir>,
) -> Result<Value<'hir>, EvalError> {
    let value = match pat.kind {
        PatKind::Lit(Lit::Ident(ident)) => {
            bindings_in_scope
                .get(&ident)
                .ok_or_else(|| EvalError::UnboundVariable {
                    literal: format!("{pat:?}"),
                    span: pat.span,
                })?
        }
        PatKind::Lit(Lit::Integer(num)) => Value::Integer(num),
        PatKind::Lit(Lit::Bool(b)) => Value::Bool(b),
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
                        .map(|pat| pat_as_expr(pat, bindings_in_scope));

                    (*field_pat, opt_value)
                }),
                bindings_in_scope,
            )?;

            Value::Record(Rc::new(fields))
        }
        PatKind::Constructor(constructor) => Value::Choice(
            constructor.ty,
            constructor.variant,
            Rc::new(pat_as_expr(constructor.kind, bindings_in_scope)?),
        ),
        PatKind::Error => todo!(),
    };

    Ok(value)
}

fn call_function<'hir>(
    left: Value<'hir>,
    function: Value<'hir>,
    right: Value<'hir>,
    global_state: &GlobalBindings<'hir>,
) -> Result<Value<'hir>, EvalError> {
    match function {
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
            let mut new_scope = (*closure_env).clone();
            let mut push_binding_fn = |ident, value| {
                new_scope.insert(ident, value);
            };
            match arm.params.len() {
                0 => {
                    // nothing to match
                }
                1 => {
                    match_pattern(left, &arm.params[0].pat, &mut push_binding_fn)?;
                }
                2 => {
                    match_pattern(left, &arm.params[0].pat, &mut push_binding_fn)?;
                    match_pattern(right, &arm.params[1].pat, &mut push_binding_fn)?;
                }
                _ => unreachable!("functions in curse cannot have more than parameters"),
            };

            let local_state = &new_scope;
            eval_expr(
                &arm.body,
                BindingsInScope {
                    global_state,
                    local_state,
                },
            )
        }
        Value::Builtin(builtin) => builtin(left, right),
        _ => Err(EvalError::TypeMismatch),
    }
}

fn check_pattern<'hir>(value: &Value, pattern: &Pat<'hir>) -> bool {
    match (&pattern.kind, value) {
        (PatKind::Record(pattern_map), Value::Record(value_map)) => {
            if pattern_map.len() != value_map.len() {
                false
            } else {
                pattern_map
                    .iter()
                    .zip(value_map.iter())
                    .all(|((_, opt_pat), (_, val))| match opt_pat {
                        Some(pat) => check_pattern(val, pat),
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
            Value::Choice(value_ty, value_variant, value),
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
    get_value: &mut dyn FnMut(InternedString) -> Option<Value<'hir>>,
    push_binding: &mut dyn FnMut(InternedString, Value<'hir>),
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
                    match_pattern(Value::Record(Rc::new(new_entries)), pat, push_binding)?;
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
        //                 (Value::Record(OwnedMap::new(new_entries))),
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
    value: Value<'hir>,
    pattern: &Pat<'hir>,
    push_binding: &mut dyn FnMut(InternedString, Value<'hir>),
) -> Result<(), EvalError> {
    if let PatKind::Lit(Lit::Ident(ident)) = pattern.kind {
        push_binding(ident, value);
        Ok(())
    } else {
        match (&pattern.kind, value) {
            (PatKind::Record(pattern_map), Value::Record(value_map)) => {
                let mut value_fields: HashMap<InternedString, Value<'hir>> = value_map
                    .iter()
                    .map(|(ident, value)| (*ident, value.clone()))
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
                Value::Choice(value_ty, value_variant, value),
            ) => {
                if *pat_ty == value_ty && *pat_variant == value_variant {
                    match_pattern((*value).clone(), kind, push_binding)
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
            (PatKind::Lit(Lit::Integer(a)), Value::Integer(b)) => {
                if *a == b {
                    Ok(())
                } else {
                    Err(EvalError::FailedPatternMatch)
                }
            }
            (PatKind::Lit(Lit::Bool(a)), Value::Bool(b)) => {
                if *a == b {
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

pub fn execute_program<'hir>(program: &Program<'hir>) -> Result<Value<'hir>, EvalError> {
    let mut global_state = GlobalBindings::new();

    for (name, def) in &program.function_defs {
        global_state.functions.insert(
            *name,
            Value::Function(def.arms.as_slice(), Rc::new(HashMap::new())),
        );
    }

    for (name, def) in &program.choice_defs {
        for (variant, _) in def.variants.iter() {
            global_state.constructors.insert(variant.symbol, *name);
        }
    }

    call_function(
        Value::default(),
        global_state
            .functions
            .get(&InternedString::get_or_intern("main"))
            .ok_or(EvalError::MissingMain)
            .cloned()?,
        Value::default(),
        &global_state,
    )
}
