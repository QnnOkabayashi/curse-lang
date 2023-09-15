use curse_hir::hir::{self, ExprKind};
use curse_interner::{Ident, InternedString};
use curse_span::Span;

use crate::{
    convert_expr,
    cpsexpr::{CPSPrimop, CPSExpr, CPSRecord, Primop, Value::*}, reset_sym_counter,
};

// MAKE SURE TO RUN AS `cargo test -- --test-threads=1`

#[test]
fn records() {
    let _interner = curse_interner::init();
    reset_sym_counter();

    let span = Span { start: 0, end: 0 };
    let parts = &[
        hir::Expr {
            kind: ExprKind::Lit(hir::Lit::Integer(1)),
            span,
        },
        hir::Expr {
            kind: ExprKind::Symbol(hir::Symbol::Plus),
            span,
        },
        hir::Expr {
            kind: ExprKind::Lit(hir::Lit::Integer(1)),
            span,
        },
    ];
    let one_plus_one = hir::Expr {
        kind: ExprKind::Appl(hir::Appl { parts }),
        span,
    };

    let four = hir::Expr {
        kind: ExprKind::Lit(hir::Lit::Integer(4)),
        span,
    };

    let true_exp = hir::Expr {
        kind: ExprKind::Lit(hir::Lit::Bool(true)),
        span,
    };

    let ident = hir::Expr {
        kind: ExprKind::Lit(hir::Lit::Ident(Ident::new("a", span))),
        span,
    };

    let map_entries = &[
        (Ident::new("x", span), Some(&one_plus_one)),
        (Ident::new("y", span), Some(&four)),
        (Ident::new("z", span), Some(&true_exp)),
        (Ident::new("w", span), Some(&ident)),
        (Ident::new("v", span), None),
    ];

    let map = hir::Map::new(map_entries);

    let map_expr = hir::Expr {
        kind: ExprKind::Record(map),
        span,
    };

    let map_cps = convert_expr(map_expr, &mut |val| CPSExpr::Halt(val));
    let t1 = InternedString::get_or_intern("record__1_");
    let t2 = InternedString::get_or_intern("t__2_");
    let a = InternedString::get_or_intern("a");
    let x = InternedString::get_or_intern("v");
    assert_eq!(
        map_cps,
        CPSExpr::Primop(CPSPrimop {
            primop: Primop::Plus,
            left: Int(1),
            right: Int(1),
            name: t2,
            continuation: Box::new(CPSExpr::Record(CPSRecord {
                values: vec![Var(t2), Int(4), Int(1), Var(a), Var(x)],
                name: t1,
                continuation: Box::new(CPSExpr::Halt(Var(t1))),
            })),
        })
    );
}

#[test]
fn basics() {
    let _interner = curse_interner::init();
    reset_sym_counter();

    let one_plus_one = hir::Expr {
        kind: ExprKind::Appl(hir::Appl {
            parts: &[
                hir::Expr {
                    kind: ExprKind::Lit(hir::Lit::Integer(1)),
                    span: Span { start: 0, end: 0 },
                },
                hir::Expr {
                    kind: ExprKind::Symbol(hir::Symbol::Plus),
                    span: Span { start: 0, end: 0 },
                },
                hir::Expr {
                    kind: ExprKind::Lit(hir::Lit::Integer(1)),
                    span: Span { start: 0, end: 0 },
                },
            ],
        }),
        span: Span { start: 0, end: 0 },
    };

    let parts = &[
        one_plus_one.clone(),
        hir::Expr {
            kind: ExprKind::Symbol(hir::Symbol::Star),
            span: Span { start: 0, end: 0 },
        },
        one_plus_one.clone(),
    ];
    let big = hir::Expr {
        kind: ExprKind::Appl(hir::Appl { parts }),
        span: Span { start: 0, end: 0 },
    };

    let t1 = InternedString::get_or_intern("t__1_");
    let t2 = InternedString::get_or_intern("t__2_");
    let t3 = InternedString::get_or_intern("t__3_");
    let one_plus_one_cps = convert_expr(big, &mut |val| CPSExpr::Halt(val));
    assert_eq!(
        one_plus_one_cps,
        CPSExpr::Primop(CPSPrimop {
            primop: Primop::Plus,
            left: Int(1),
            right: Int(1),
            name: t1,
            continuation: Box::new(CPSExpr::Primop(CPSPrimop {
                primop: Primop::Plus,
                left: Int(1),
                right: Int(1),
                name: t2,
                continuation: Box::new(CPSExpr::Primop(CPSPrimop {
                    primop: Primop::Times,
                    left: Var(t1),
                    right: Var(t2),
                    name: t3,
                    continuation: Box::new(CPSExpr::Halt(Var(t3)))
                }))
            }))
        })
    );
}
