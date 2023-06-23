#![allow(dead_code)]

use std::collections::HashMap;

use curse_hir::{ExprRef, Lit, Program};
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
        curse_hir::ExprKind::Symbol(_) => todo!(),
        curse_hir::ExprKind::Lit(Lit::Integer(int)) => Ok(Value::Integer(int)),
        curse_hir::ExprKind::Lit(Lit::Ident(ident)) => local_state
            .get(&ident)
            .or(global_state.functions.get(&ident.string))
            .ok_or(EvalError::UnboundVariable {
                literal: ident.to_string(),
                span: ident.span,
            })
            .cloned(),
        curse_hir::ExprKind::Lit(Lit::Bool(bool)) => Ok(Value::Bool(bool)),
        curse_hir::ExprKind::Record(_) => todo!(),
        curse_hir::ExprKind::Constructor(_, _) => todo!(),
        curse_hir::ExprKind::Closure(_) => todo!(),
        curse_hir::ExprKind::Appl(_) => todo!(),
        curse_hir::ExprKind::Error => todo!(),
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
    );

    Ok(())
}
