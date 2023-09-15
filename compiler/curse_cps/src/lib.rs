// just for now
#![allow(dead_code)]

use std::{cell::RefCell, rc::Rc};

use cpsexpr::{
    var, var_from_id, CPSAppl, CPSExpr, CPSPrimop, CPSRecord, CPSFix, Function, Primop, Value,
};
use curse_hir::hir::{self, ExprKind};
use curse_interner::InternedString;

pub mod cpsexpr;

#[cfg(test)]
mod tests;

static mut CURRENT_ID: usize = 0;

fn gensym(s: &str) -> InternedString {
    // we're single threaded, so this is safe
    // if we ever upgrade to multithreading, we can just replace this with an `AtomicUsize`
    unsafe {
        CURRENT_ID += 1;
        InternedString::get_or_intern(&format!("{}__{}_", s, CURRENT_ID))
    }
}

fn reset_sym_counter() {
    unsafe {
        CURRENT_ID = 0;
    }
}

fn symbol_to_primop(symbol: hir::Symbol) -> Primop {
    match symbol {
        hir::Symbol::Plus => Primop::Plus,
        hir::Symbol::Minus => Primop::Minus,
        hir::Symbol::Star => Primop::Times,
        hir::Symbol::Dot => todo!(),
        hir::Symbol::DotDot => todo!(),
        hir::Symbol::Semi => Primop::Semi,
        hir::Symbol::Percent => Primop::Mod,
        hir::Symbol::Slash => Primop::Div,
        hir::Symbol::Eq => Primop::Eq,
        hir::Symbol::Lt => Primop::Lt,
        hir::Symbol::Gt => Primop::Gt,
        hir::Symbol::Le => Primop::Le,
        hir::Symbol::Ge => Primop::Ge,
    }
}

fn branching_symbol(symbol: hir::Symbol) -> bool {
    match symbol {
        hir::Symbol::Eq | hir::Symbol::Lt | hir::Symbol::Gt | hir::Symbol::Le | hir::Symbol::Ge => {
            true
        }
        _ => false,
    }
}

fn convert_expr(expr: hir::Expr, cont: &mut dyn FnMut(Value) -> CPSExpr) -> CPSExpr {
    match expr.kind {
        ExprKind::Symbol(symb) => {
            let x = gensym("x");
            let y = gensym("y");
            let f = gensym("f");
            let t = gensym("t");
            CPSFix::new(
                vec![Function::new(
                    Value::Var(x),
                    Value::Var(f),
                    Value::Var(y),
                    Box::new(CPSPrimop::new(
                        symbol_to_primop(symb),
                        Value::Var(x),
                        Value::Var(y),
                        t,
                        vec![cont(Value::Var(t))],
                    )),
                )],
                Box::new(cont(Value::Var(f))),
            )
        }
        ExprKind::Lit(hir::Lit::Integer(n)) => cont(Value::Int(n)),
        ExprKind::Lit(hir::Lit::Bool(true)) => cont(Value::Int(1)),
        ExprKind::Lit(hir::Lit::Bool(false)) => cont(Value::Int(0)),
        ExprKind::Lit(hir::Lit::Ident(var)) => cont(var_from_id(var)),
        ExprKind::Record(map) => {
            let results = vec![];
            let name = gensym("record");
            let map = map.entries.iter().map(|(x, y)| (*x, y.cloned())).collect();
            convert_record(
                Rc::new(map),
                Rc::new(RefCell::new(results)),
                Rc::new(RefCell::new(&mut |values: Rc<RefCell<Vec<Value>>>| {
                    CPSRecord::new(
                        std::mem::take(&mut values.borrow_mut()),
                        name,
                        Box::new(cont(Value::Var(name))),
                    )
                })),
                0,
            )
        }
        // represent a constructor as a record with a string tag and value
        // TODO(william) represent tag with an integer (requires knowing all the variants in each
        // enum)
        ExprKind::Constructor(hir::Constructor { path, inner }) => {
            let name = gensym("ctor");
            convert_expr(*inner, &mut |inner_val| {
                CPSRecord::new(
                    vec![var(&format!("{path:?}")), inner_val],
                    name,
                    Box::new(cont(Value::Var(name))),
                )
            })
        }
        ExprKind::Closure(_) => todo!(),
        ExprKind::Appl(appl) => match appl.fun().kind {
            ExprKind::Symbol(symb) => {
                if branching_symbol(symb) {
                    convert_expr(*appl.lhs(), &mut |lhs| {
                        convert_expr(*appl.rhs(), &mut |rhs| {
                            let x = Value::Var(gensym("x"));
                            let y = Value::Var(gensym("y"));
                            let k = Value::Var(gensym("k"));
                            let b = gensym("b");
                            CPSFix::new(
                                vec![Function::new(x, k, y, Box::new(cont(x)))],
                                Box::new(CPSPrimop::new(
                                    symbol_to_primop(symb),
                                    lhs,
                                    rhs,
                                    b,
                                    vec![
                                        CPSAppl::new(k, vec![Value::Int(1)]),
                                        CPSAppl::new(k, vec![Value::Int(0)]),
                                    ],
                                )),
                            )
                        })
                    })
                } else {
                    convert_expr(*appl.lhs(), &mut |lhs| {
                        convert_expr(*appl.rhs(), &mut |rhs| {
                            let t = gensym("t");
                            CPSPrimop::new(
                                symbol_to_primop(symb),
                                lhs,
                                rhs,
                                t,
                                vec![cont(Value::Var(t))],
                            )
                        })
                    })
                }
            }
            _ => {
                let x = Value::Var(gensym("x"));
                let r = Value::Var(gensym("r"));
                CPSFix::new(
                    vec![Function::new(x, r, Value::Int(0), Box::new(cont(x)))],
                    Box::new(convert_expr(*appl.fun(), &mut |f| {
                        convert_expr(*appl.lhs(), &mut |lhs| {
                            convert_expr(*appl.rhs(), &mut |rhs| CPSAppl::new(f, vec![lhs, rhs, r]))
                        })
                    })),
                )
            }
        },
        ExprKind::Region(_) => todo!(),
        ExprKind::Error => todo!(),
    }
}

// pls make this better Quinn
// TODO(william) we should sort the fields in records alphabetically to guarantee that entries of
// records of the same type will line up regardless of the order of the fields
fn convert_record(
    map_vec: Rc<Vec<(curse_interner::Ident, Option<hir::Expr>)>>,
    current_vec: Rc<RefCell<Vec<Value>>>,
    cont: Rc<RefCell<&mut dyn FnMut(Rc<RefCell<Vec<Value>>>) -> CPSExpr>>,
    map_index: usize,
) -> CPSExpr {
    match map_vec.get(map_index) {
        Some((_ident, Some(expr))) => convert_expr(*expr, &mut |v| {
            current_vec.borrow_mut().push(v);
            convert_record(
                map_vec.clone(),
                current_vec.clone(),
                cont.clone(),
                map_index + 1,
            )
        }),
        Some((ident, None)) => {
            current_vec.borrow_mut().push(var_from_id(*ident));
            convert_record(map_vec, current_vec, cont, map_index + 1)
        }
        None => (cont.borrow_mut())(current_vec.clone()),
    }
}
