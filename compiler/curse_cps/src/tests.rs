use curse_hir::hir::{self, ExprKind};
use curse_interner::{Ident, InternedString};
use curse_span::Span;

use crate::{
    convert_expr,
    cpsexpr::{var, Appl, CPSExpr, CPSPrimop, CPSRecord, Fix, Function, Primop, Value::*},
    reset_sym_counter,
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
            continuations: vec![CPSExpr::Record(CPSRecord {
                values: vec![Var(t2), Int(4), Int(1), Var(a), Var(x)],
                name: t1,
                continuation: Box::new(CPSExpr::Halt(Var(t1))),
            })],
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
            continuations: vec![CPSExpr::Primop(CPSPrimop {
                primop: Primop::Plus,
                left: Int(1),
                right: Int(1),
                name: t2,
                continuations: vec![CPSExpr::Primop(CPSPrimop {
                    primop: Primop::Times,
                    left: Var(t1),
                    right: Var(t2),
                    name: t3,
                    continuations: vec![CPSExpr::Halt(Var(t3))]
                })]
            })]
        })
    );
}

#[test]
fn appl() {
    let _interner = curse_interner::init();
    reset_sym_counter();

    let span = Span { start: 0, end: 0 };

    let inner_parts = [
        hir::Expr {
            kind: ExprKind::Lit(hir::Lit::Integer(1)),
            span,
        },
        hir::Expr {
            kind: ExprKind::Lit(hir::Lit::Ident(Ident::new("range", span))),
            span,
        },
        hir::Expr {
            kind: ExprKind::Lit(hir::Lit::Integer(100)),
            span,
        },
    ];
    let parts = [
        hir::Expr {
            kind: ExprKind::Appl(hir::Appl {
                parts: &inner_parts,
            }),
            span,
        },
        hir::Expr {
            kind: ExprKind::Lit(hir::Lit::Ident(Ident::new("in", span))),
            span,
        },
        hir::Expr {
            kind: ExprKind::Lit(hir::Lit::Ident(Ident::new("sum", span))),
            span,
        },
    ];
    let expr = hir::Expr {
        kind: ExprKind::Appl(hir::Appl { parts: &parts }),
        span,
    };

    let cps_expr = convert_expr(expr, &mut |val| CPSExpr::Halt(val));

    let expected = CPSExpr::Fix(Fix {
        functions: vec![Function {
            left: var("x__1_"),
            name: var("r__2_"),
            right: Int(0),
            continuation: Box::new(CPSExpr::Halt(var("x__1_"))),
        }],
        continuation: Box::new(CPSExpr::Fix(Fix {
            functions: vec![Function {
                left: var("x__3_"),
                name: var("r__4_"),
                right: Int(0),
                continuation: Box::new(CPSExpr::Appl(Appl {
                    function: var("in"),
                    args: vec![var("x__3_"), var("sum"), var("r__2_")],
                })),
            }],
            continuation: Box::new(CPSExpr::Appl(Appl {
                function: var("range"),
                args: vec![Int(1), Int(100), var("r__4_")],
            })),
        })),
    });

    assert_eq!(cps_expr, expected);
}

#[test]
fn symb() {
    let _interner = curse_interner::init();
    reset_sym_counter();

    let expr = hir::Expr {
        kind: ExprKind::Symbol(hir::Symbol::Plus),
        span: Span { start: 0, end: 0 },
    };

    let cps = convert_expr(expr, &mut |val| CPSExpr::Halt(val));
    let expected = CPSExpr::Fix(Fix {
        functions: vec![Function {
            left: var("x__1_"),
            name: var("f__3_"),
            right: var("y__2_"),
            continuation: Box::new(CPSExpr::Primop(CPSPrimop {
                primop: Primop::Plus,
                left: var("x__1_"),
                right: var("y__2_"),
                name: InternedString::get_or_intern("t__4_"),
                continuations: vec![CPSExpr::Halt(var("t__4_"))],
            })),
        }],
        continuation: Box::new(CPSExpr::Halt(var("f__3_"))),
    });

    assert_eq!(cps, expected);
}

#[test]
fn ctor() {
    let _interner = curse_interner::init();
    let span = Span { start: 0, end: 0 };
    reset_sym_counter();

    let four = hir::Expr {
        kind: ExprKind::Lit(hir::Lit::Integer(4)),
        span,
    };
    let parts = [
        four,
        hir::Expr {
            kind: ExprKind::Symbol(hir::Symbol::Plus),
            span,
        },
        four,
    ];
    let inner = hir::Expr {
        kind: ExprKind::Appl(hir::Appl { parts: &parts }),
        span,
    };
    let path = [Ident::new("Option", span), Ident::new("Some", span)];
    let ctor_expr = hir::Expr {
        kind: ExprKind::Constructor(hir::Constructor {
            path: &path,
            inner: &inner,
        }),
        span,
    };

    let ctor_cps = convert_expr(ctor_expr, &mut |val| CPSExpr::Halt(val));
    let expected = CPSExpr::Primop(CPSPrimop {
        primop: Primop::Plus,
        left: Int(4),
        right: Int(4),
        name: InternedString::get_or_intern("t__2_"),
        continuations: vec![CPSExpr::Record(CPSRecord {
            values: vec![var("[Option, Some]"), var("t__2_")],
            name: InternedString::get_or_intern("ctor__1_"),
            continuation: Box::new(CPSExpr::Halt(var("ctor__1_"))),
        })],
    });

    assert_eq!(ctor_cps, expected);
}

#[test]
fn branching() {
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
            kind: ExprKind::Symbol(hir::Symbol::Eq),
            span: Span { start: 0, end: 0 },
        },
        one_plus_one.clone(),
    ];
    let big = hir::Expr {
        kind: ExprKind::Appl(hir::Appl { parts }),
        span: Span { start: 0, end: 0 },
    };

    let one_plus_one_cps = convert_expr(big, &mut |val| CPSExpr::Halt(val));

    let t1 = InternedString::get_or_intern("t__1_");
    let t2 = InternedString::get_or_intern("t__2_");
    let x = InternedString::get_or_intern("x__3_");
    let y = InternedString::get_or_intern("y__4_");
    let k = InternedString::get_or_intern("k__5_");
    let b = InternedString::get_or_intern("b__6_");

    assert_eq!(
        one_plus_one_cps,
        CPSExpr::Primop(CPSPrimop {
            primop: Primop::Plus,
            left: Int(1),
            right: Int(1),
            name: t1,
            continuations: vec![CPSExpr::Primop(CPSPrimop {
                primop: Primop::Plus,
                left: Int(1),
                right: Int(1),
                name: t2,
                continuations: vec![CPSExpr::Fix(Fix {
                    functions: vec![Function {
                        left: Var(x),
                        name: Var(k),
                        right: Var(y),
                        continuation: Box::new(CPSExpr::Halt(Var(x))),
                    }],
                    continuation: Box::new(CPSExpr::Primop(CPSPrimop {
                        primop: Primop::Eq,
                        left: Var(t1),
                        right: Var(t2),
                        name: b,
                        continuations: vec![
                            CPSExpr::Appl(Appl {
                                function: Var(k),
                                args: vec![Int(1)],
                            }),
                            CPSExpr::Appl(Appl {
                                function: Var(k),
                                args: vec![Int(0)],
                            }),
                        ],
                    })),
                })],
            })],
        })
    );
}
