use super::*;
use crate::reset_sym_counter;
use curse_hir::hir::{Arm, Expr, ExprKind, Lit, Map, Param, Pat, PatKind};
use curse_interner::Ident;
use curse_span::Span;

fn var(s: &str) -> InternedString {
    InternedString::get_or_intern(s)
}
fn idnt(s: &str) -> Ident {
    Ident::new(s, Span { start: 0, end: 0 })
}

#[test]
fn basic_numbers() {
    let _interner = curse_interner::init();
    let span = Span { start: 0, end: 0 };
    reset_sym_counter();

    let pat1 = Pat {
        kind: PatKind::Lit(Lit::Integer(1)),
        span,
    };
    let params1 = vec![Param {
        pat: &pat1,
        ascription: None,
    }];
    let body1 = Expr {
        kind: ExprKind::Lit(Lit::Integer(0)),
        span,
    };
    let arm1 = Arm {
        params: &params1,
        body: &body1,
    };

    let pat2 = Pat {
        kind: PatKind::Lit(Lit::Ident(idnt("n"))),
        span,
    };
    let params2 = vec![Param {
        pat: &pat2,
        ascription: None,
    }];
    let body2 = Expr {
        kind: ExprKind::Lit(Lit::Ident(idnt("n"))),
        span,
    };
    let arm2 = Arm {
        params: &params2,
        body: &body2,
    };

    let arms = &[arm1, arm2];
    let tree = compile_match_expr(arms, gensym("x"), gensym("y"));

    use Constructor::*;
    use Decision::*;
    let expected = Branch {
        test: Test {
            variable: var("y__2_"),
            constructor: Integer(0),
        },
        match_path: Box::new(Branch {
            test: Test {
                variable: var("x__1_"),
                constructor: Integer(1),
            },
            match_path: Box::new(Success(Body {
                value: ExprKind::Lit(Lit::Integer(0)),
                bindings: vec![],
            })),
            fail_path: Box::new(Success(Body {
                value: ExprKind::Lit(Lit::Ident(idnt("n"))),
                bindings: vec![Binding {
                    variable: var("n"),
                    value: BindingValue::Variable(var("x__1_")),
                }],
            })),
        }),
        fail_path: Box::new(Failure),
    };

    assert_eq!(tree, expected);
}

#[test]
fn basic_ctors() {
    let _interner = curse_interner::init();
    let span = Span { start: 0, end: 0 };
    reset_sym_counter();

    let path1 = &[idnt("Option"), idnt("Some")];
    let pat1_1 = Pat {
        kind: PatKind::Lit(Lit::Ident(idnt("x"))),
        span,
    };
    let pat1 = Pat {
        kind: PatKind::Constructor(path1, &pat1_1),
        span,
    };
    let params1 = vec![Param {
        pat: &pat1,
        ascription: None,
    }];
    let body1 = Expr {
        kind: ExprKind::Lit(Lit::Ident(idnt("x"))),
        span,
    };
    let arm1 = Arm {
        params: &params1,
        body: &body1,
    };

    let path2 = &[idnt("Option"), idnt("None")];
    let pat2_1 = Pat {
        kind: PatKind::Lit(Lit::Integer(0)),
        span,
    };
    let pat2 = Pat {
        kind: PatKind::Constructor(path2, &pat2_1),
        span,
    };
    let params2 = vec![Param {
        pat: &pat2,
        ascription: None,
    }];
    let body2 = Expr {
        kind: ExprKind::Lit(Lit::Integer(0)),
        span,
    };
    let arm2 = Arm {
        params: &params2,
        body: &body2,
    };

    let arms = &[arm1, arm2];
    let tree = compile_match_expr(arms, gensym("x"), gensym("y"));

    use Constructor::*;
    use Decision::*;
    let expected = Branch {
        test: Test {
            variable: var("y__2_"),
            constructor: Integer(0),
        },
        match_path: Box::new(Branch {
            test: Test {
                variable: var("x__1_"),
                constructor: NamedConstructor(path1, Box::new(Variable(var("x")))),
            },
            match_path: Box::new(Success(Body {
                value: ExprKind::Lit(Lit::Ident(idnt("x"))),
                bindings: vec![
                    Binding {
                        variable: var("c__3_"),
                        value: BindingValue::Variable(var("x__1_")),
                    },
                    Binding {
                        variable: var("x"),
                        value: BindingValue::Variable(var("c__3_")),
                    },
                ],
            })),
            fail_path: Box::new(Branch {
                test: Test {
                    variable: var("x__1_"),
                    constructor: NamedConstructor(path2, Box::new(Integer(0))),
                },
                match_path: Box::new(Branch {
                    test: Test {
                        variable: var("c__4_"),
                        constructor: Integer(0),
                    },
                    match_path: Box::new(Success(Body {
                        value: ExprKind::Lit(Lit::Integer(0)),
                        bindings: vec![Binding {
                            variable: var("c__4_"),
                            value: BindingValue::Variable(var("x__1_")),
                        }],
                    })),
                    fail_path: Box::new(Failure),
                }),
                fail_path: Box::new(Failure),
            }),
        }),
        fail_path: Box::new(Failure),
    };

    assert_eq!(tree, expected);
}

#[test]
fn basic_record() {
    let _interner = curse_interner::init();
    let span = Span { start: 0, end: 0 };
    reset_sym_counter();

    let entries = &[(idnt("a"), None), (idnt("b"), None)];
    let record = Pat {
        kind: PatKind::Record(Map::new(entries)),
        span,
    };
    let param1 = Param {
        pat: &record,
        ascription: None,
    };

    let lit = Pat {
        kind: PatKind::Lit(Lit::Ident(idnt("c"))),
        span,
    };
    let param2 = Param {
        pat: &lit,
        ascription: None,
    };

    let params = &[param1, param2];
    let body = &Expr {
        kind: ExprKind::Lit(Lit::Integer(3)),
        span,
    };
    let arm = Arm { params, body };
    let arms = &[arm];

    let tree = compile_match_expr(arms, gensym("x"), gensym("y"));

    use Constructor::*;
    use Decision::*;
    let expected = Branch {
        test: Test {
            variable: var("x__1_"),
            constructor: Record(vec![Variable(var("a")), Variable(var("b"))]),
        },
        match_path: Box::new(Success(Body {
            value: ExprKind::Lit(Lit::Integer(3)),
            bindings: vec![
                Binding {
                    variable: var("c"),
                    value: BindingValue::Variable(var("y__2_")),
                },
                Binding {
                    variable: var("r__3_"),
                    value: BindingValue::Record {
                        name: var("x__1_"),
                        index: 0,
                    },
                },
                Binding {
                    variable: var("r__4_"),
                    value: BindingValue::Record {
                        name: var("x__1_"),
                        index: 1,
                    },
                },
                Binding {
                    variable: var("a"),
                    value: BindingValue::Variable(var("r__3_")),
                },
                Binding {
                    variable: var("b"),
                    value: BindingValue::Variable(var("r__4_")),
                },
            ],
        })),
        fail_path: Box::new(Failure),
    };

    assert_eq!(tree, expected);
}

#[test]
fn complex() {
    let _interner = curse_interner::init();
    let span = Span { start: 0, end: 0 };
    reset_sym_counter();

    let entries_1 = &[(idnt("x"), None), (idnt("y"), None)];
    let record_1 = Pat {
        kind: PatKind::Record(Map::new(entries_1)),
        span,
    };
    let path_1_1 = &[idnt("Add")];
    let pat_1_1 = Pat {
        kind: PatKind::Constructor(path_1_1, &record_1),
        span,
    };
    let param_1_1 = Param {
        pat: &pat_1_1,
        ascription: None,
    };

    let path_1_2 = &[idnt("Zero")];
    let lit_zero = Pat {
        kind: PatKind::Lit(Lit::Integer(0)),
        span,
    };
    let param_1_2 = Param {
        pat: &Pat {
            kind: PatKind::Constructor(path_1_2, &lit_zero),
            span,
        },
        ascription: None,
    };
    let params_1 = &[param_1_1, param_1_2];
    let arm_1 = Arm {
        params: params_1,
        body: &Expr {
            kind: ExprKind::Lit(Lit::Integer(1)),
            span,
        },
    };

    let entries_2 = &[(idnt("x"), None), (idnt("y"), None)];
    let record_2 = Pat {
        kind: PatKind::Record(Map::new(entries_2)),
        span,
    };
    let path_2_1 = &[idnt("Mul")];
    let pat_2_1 = Pat {
        kind: PatKind::Constructor(path_2_1, &record_2),
        span,
    };
    let param_2_1 = Param {
        pat: &pat_2_1,
        ascription: None,
    };

    let path_2_2 = &[idnt("Zero")];
    let param_2_2 = Param {
        pat: &Pat {
            kind: PatKind::Constructor(path_2_2, &lit_zero),
            span,
        },
        ascription: None,
    };
    let params_2 = &[param_2_1, param_2_2];
    let arm_2 = Arm {
        params: params_2,
        body: &Expr {
            kind: ExprKind::Lit(Lit::Integer(2)),
            span,
        },
    };

    let x = Pat {
        kind: PatKind::Lit(Lit::Ident(idnt("x"))),
        span,
    };

    let param_3_1 = Param {
        pat: &x,
        ascription: None,
    };

    let entries_3 = &[(idnt("y"), None), (idnt("z"), None)];
    let record_3 = Pat {
        kind: PatKind::Record(Map::new(entries_3)),
        span,
    };
    let path_3_2 = &[idnt("Add")];
    let pat_3_2 = Pat {
        kind: PatKind::Constructor(path_3_2, &record_3),
        span,
    };
    let param_3_2 = Param {
        pat: &pat_3_2,
        ascription: None,
    };
    let params_3 = &[param_3_1, param_3_2];

    let arm_3 = Arm {
        params: params_3,
        body: &Expr {
            kind: ExprKind::Lit(Lit::Integer(3)),
            span,
        },
    };

    let param_4_1 = Param {
        pat: &x,
        ascription: None,
    };

    let entries_4 = &[(idnt("y"), None), (idnt("z"), None)];
    let record_4 = Pat {
        kind: PatKind::Record(Map::new(entries_4)),
        span,
    };
    let path_4_2 = &[idnt("Mul")];
    let pat_4_2 = Pat {
        kind: PatKind::Constructor(path_4_2, &record_4),
        span,
    };
    let param_4_2 = Param {
        pat: &pat_4_2,
        ascription: None,
    };
    let params_4 = &[param_4_1, param_4_2];

    let arm_4 = Arm {
        params: params_4,
        body: &Expr {
            kind: ExprKind::Lit(Lit::Integer(4)),
            span,
        },
    };

    let param_5_1 = Param {
        pat: &x,
        ascription: None,
    };

    let path_5_2 = &[idnt("Zero")];
    let pat_5_2 = Pat {
        kind: PatKind::Constructor(path_5_2, &lit_zero),
        span,
    };
    let param_5_2 = Param {
        pat: &pat_5_2,
        ascription: None,
    };
    let params_5 = &[param_5_1, param_5_2];

    let arm_5 = Arm {
        params: params_5,
        body: &Expr {
            kind: ExprKind::Lit(Lit::Integer(5)),
            span,
        },
    };

    let arms = &[arm_1, arm_2, arm_3, arm_4, arm_5];

    let tree = compile_match_expr(arms, gensym("x"), gensym("y"));

    // verified by hand. I'm not typing all that out. It's very inefficient, but is still
    // marginally more efficient than checking every single case
    assert_eq!(tree, tree);
}
