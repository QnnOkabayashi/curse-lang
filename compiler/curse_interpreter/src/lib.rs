#![allow(dead_code)]

use std::collections::HashMap;

use curse_hir::hir::{ExprRef, Lit, Program, ExprKind};
use curse_interner::{Ident, InternedString};
use error::EvalError;
use value::Value;
mod error;
mod value;

// globally available functions, both regular named functions as well as type constructors
struct GlobalBindings<'hir> {
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

type Bindings<'hir> = HashMap<Ident, Value<'hir>>;

fn eval_expr<'hir>(
    expr: ExprRef<'hir>,
    global_state: &GlobalBindings<'hir>,
    local_state: &mut Bindings<'hir>,
) -> Result<Value<'hir>, EvalError> {
    match expr.kind {
        ExprKind::Symbol(_) => todo!(),
        ExprKind::Lit(Lit::Integer(int)) => Ok(Value::Integer(int)),
        ExprKind::Lit(Lit::Ident(ident)) => local_state
            .get(&ident)
            .or(global_state.functions.get(&ident.string))
            .ok_or(EvalError::UnboundVariable {
                literal: ident.to_string(),
                span: ident.span,
            })
            .cloned(),
        ExprKind::Lit(Lit::Bool(bool)) => Ok(Value::Bool(bool)),
        ExprKind::Record(_) => todo!(),
        ExprKind::Constructor(_) => todo!(),
        ExprKind::Closure(_) => todo!(),
        ExprKind::Appl(_) => todo!(),
        ExprKind::Error => todo!(),
    }
}

fn call_function<'hir>(
    left: &Value,
    function: &Value,
    right: &Value,
    global_state: &GlobalBindings,
    local_state: &mut Bindings,
) -> Result<Value<'hir>, EvalError> {
    let Value::Function(arms) = function else { unreachable!("thanks type checking") };

    todo!();
}

pub fn execute_program(program: &Program) -> Result<(), EvalError> {
    let mut global_state = GlobalBindings::new();

    for (name, def) in &program.function_defs {
        global_state
            .functions
            .insert(*name, Value::Function(def.function));
    }

    for (name, def) in &program.choice_defs {
        for (variant, _) in def.variants.entries {
            global_state.constructors.insert(variant.string, *name);
        }
    }

    let mut local_state = HashMap::new();

    call_function(
        &Value::default(),
        global_state
            .functions
            .get(&InternedString::new("main"))
            .ok_or(EvalError::MissingMain)?,
        &Value::default(),
        &global_state,
        &mut local_state,
    )?;

    Ok(())
}
