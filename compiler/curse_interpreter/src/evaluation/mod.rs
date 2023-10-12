use crate::builtins::Builtin;
use crate::error::{EvalError, MatchError};
use crate::value::Value;
use curse_hir::hir::{Constructor, Expr, ExprKind, Lit, Pat, PatKind, Program, Symbol};
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
struct RuntimeState<'a, 'hir> {
    global_state: &'a GlobalBindings<'hir>,
    local_state: &'a Bindings<'hir>,
}

impl<'a, 'hir> RuntimeState<'a, 'hir> {
    fn get(self, ident: &InternedString) -> Option<Value<'hir>> {
        self.local_state
            .get(ident)
            .or_else(|| self.global_state.functions.get(ident))
            .cloned()
    }

    fn eval_record(
        self,
        fields: &[(Pat<'hir>, Option<Expr<'hir>>)],
    ) -> Result<Vec<(InternedString, Value<'hir>)>, EvalError> {
        let mut entries = vec![];

        for (field_pat, opt_expr) in fields {
            if let Some(expr) = opt_expr {
                let value = self.eval_expr(expr)?;
                bind_pat_against_value(value, &field_pat, &mut |ident, value| {
                    entries.push((ident, value))
                })?;
            } else {
                match field_pat.kind {
                    PatKind::Lit(Lit::Ident(ident)) => {
                        let value = self.get(&ident).ok_or_else(|| EvalError::UnboundVariable {
                            literal: ident.to_string(),
                            span: field_pat.span,
                        })?;
                        entries.push((ident, value));
                    }
                    PatKind::Lit(Lit::Integer(_)) => {
                        return Err(EvalError::Match(MatchError::IntLiteralField(
                            field_pat.span.start_len().into(),
                        )))
                    }
                    PatKind::Lit(Lit::Bool(_)) => {
                        return Err(EvalError::Match(MatchError::BoolLiteralField(
                            field_pat.span.start_len().into(),
                        )))
                    }
                    PatKind::Record(_) => {
                        return Err(EvalError::Match(MatchError::RecordFieldPatWithoutValue(
                            field_pat.span.start_len().into(),
                        )))
                    }
                    PatKind::Constructor(_) => todo!(),
                    PatKind::Error => todo!(),
                }
            }
        }

        Ok(entries)
    }

    fn eval_expr(self, expr: &Expr<'hir>) -> Result<Value<'hir>, EvalError> {
        match expr.kind {
            ExprKind::Symbol(Symbol::Plus) => Ok(Value::Builtin(Builtin::Add)),
            ExprKind::Symbol(Symbol::Star) => Ok(Value::Builtin(Builtin::Mul)),
            ExprKind::Symbol(Symbol::Minus) => Ok(Value::Builtin(Builtin::Sub)),
            ExprKind::Symbol(Symbol::Slash) => Ok(Value::Builtin(Builtin::Div)),
            ExprKind::Symbol(Symbol::Percent) => Ok(Value::Builtin(Builtin::Mod)),
            ExprKind::Symbol(Symbol::Eq) => Ok(Value::Builtin(Builtin::Eq)),
            ExprKind::Symbol(Symbol::Lt) => Ok(Value::Builtin(Builtin::Lt)),
            ExprKind::Symbol(Symbol::Gt) => Ok(Value::Builtin(Builtin::Gt)),
            ExprKind::Symbol(Symbol::Le) => Ok(Value::Builtin(Builtin::Le)),
            ExprKind::Symbol(Symbol::Ge) => Ok(Value::Builtin(Builtin::Ge)),
            ExprKind::Symbol(Symbol::Semi) => Ok(Value::Builtin(Builtin::Semi)),
            ExprKind::Symbol(Symbol::Dot) => unimplemented!(),
            ExprKind::Symbol(Symbol::DotDot) => unimplemented!(),
            ExprKind::Lit(Lit::Integer(int)) => Ok(Value::Integer(int)),
            ExprKind::Lit(Lit::Ident(ident)) => {
                self.get(&ident).ok_or_else(|| EvalError::UnboundVariable {
                    literal: ident.to_string(),
                    span: expr.span,
                })
            }
            ExprKind::Lit(Lit::Bool(bool)) => Ok(Value::Bool(bool)),
            ExprKind::Record(entries) => {
                let fields = self.eval_record(entries.as_slice())?;

                Ok(Value::Record(Rc::new(fields)))
            }
            ExprKind::Constructor(constructor) => Ok(Value::Choice(
                constructor.ty,
                constructor.variant,
                Rc::new(self.eval_expr(constructor.kind)?),
            )),
            ExprKind::Closure(arms) => Ok(Value::Function(
                arms.as_slice(),
                Rc::new(self.local_state.clone()),
            )),
            ExprKind::Appl(appl) => {
                let lhs = self.eval_expr(&appl.lhs)?;
                let fun = self.eval_expr(&appl.fun)?;
                let rhs = self.eval_expr(&appl.rhs)?;
                call_function(lhs, fun, rhs, self.global_state)
            }
            ExprKind::Region(_) => todo!("Regions"),
            ExprKind::Error => todo!("error handling"),
        }
    }
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
                    bind_pat_against_value(left, &arm.params[0].pat, &mut push_binding_fn)?;
                }
                2 => {
                    bind_pat_against_value(left, &arm.params[0].pat, &mut push_binding_fn)?;
                    bind_pat_against_value(right, &arm.params[1].pat, &mut push_binding_fn)?;
                }
                _ => unreachable!("functions in curse cannot have more than parameters"),
            };

            RuntimeState {
                global_state,
                local_state: &new_scope,
            }
            .eval_expr(&arm.body)
        }
        Value::Builtin(op) => op.compute(left, right),
        uncallable => Err(EvalError::TypeMismatch(format!(
            "expected function or builtin, found {uncallable:?}"
        ))),
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
            PatKind::Constructor(Constructor {
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
    bindings: &[(Pat<'hir>, Option<Pat<'hir>>)],
    // When matching on another record, get_value looks up keys from the other record
    // When constructing, it pulls idents from the local/global bindings
    get_value: &mut dyn FnMut(InternedString) -> Option<Value<'hir>>,
    push_binding: &mut dyn FnMut(InternedString, Value<'hir>),
) -> Result<(), EvalError> {
    for (field_pat, opt_value_pat) in bindings {
        match field_pat.kind {
            PatKind::Lit(Lit::Ident(ident)) => {
                // BUG(quinn): when matching a record on another record,
                // this is the correct behavior, e.g. `{ a: b }` should
                // take the value `a` and assign it to a new variable, `b`.
                // But when constructing a record from the environment,
                // we want the field to be called `a` and to take on the
                // value `b` from the environment.
                // Looks like we're going to have to split this into two
                // functions.
                let value = get_value(ident).ok_or(EvalError::Debug(1))?;

                if let Some(value_pat) = opt_value_pat {
                    bind_pat_against_value(value, value_pat, push_binding)?;
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
                    bind_pat_against_value(Value::Record(Rc::new(new_entries)), pat, push_binding)?;
                } else {
                    // `{ { a, b } }: { a, b }`
                    // kinda pointless, just inline a and b directly into the parent
                    match_record(bindings, get_value, push_binding)?;
                }
            }
            PatKind::Constructor(_) => todo!(),
            PatKind::Error => todo!(),
        }
    }

    Ok(())
}

fn bind_pat_against_value<'hir>(
    value: Value<'hir>,
    pattern: &Pat<'hir>,
    push_binding: &mut dyn FnMut(InternedString, Value<'hir>),
) -> Result<(), EvalError> {
    match (pattern.kind, value) {
        (PatKind::Lit(Lit::Ident(ident)), value) => {
            push_binding(ident, value);
            Ok(())
        }
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
            PatKind::Constructor(Constructor {
                ty: pat_ty,
                variant: pat_variant,
                kind,
            }),
            Value::Choice(value_ty, value_variant, value),
        ) => {
            if *pat_ty == value_ty && *pat_variant == value_variant {
                bind_pat_against_value((*value).clone(), kind, push_binding)
            } else {
                Err(EvalError::FailedPatternMatch)
            }
        }
        (PatKind::Lit(Lit::Integer(a)), Value::Integer(b)) => {
            if a == b {
                Ok(())
            } else {
                Err(EvalError::FailedPatternMatch)
            }
        }
        (PatKind::Lit(Lit::Bool(a)), Value::Bool(b)) => {
            if a == b {
                Ok(())
            } else {
                Err(EvalError::FailedPatternMatch)
            }
        }
        _ => Err(EvalError::FailedPatternMatch),
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
