use super::*;
use crate::reset_sym_counter;
use bumpalo::Bump;
use curse_hir::hir::{ExprKind, Lit};
use curse_interner::Ident;
use curse_span::Span;
use Decision::*;

fn var(s: &str) -> InternedString {
    InternedString::get_or_intern(s)
}
fn idnt(s: &str) -> Ident {
    Ident::new(s, Span { start: 0, end: 0 })
}

fn get_decision<'a>(input: &str, arena: &'a Bump) -> Decision<'a> {
    let mut interner = curse_interner::init().unwrap();
    let mut parser = curse_parse::Parser::new(&mut interner);
    let ast_program = parser.parse_program(input);

    curse_interner::replace(Some(interner));

    let mut lowerer = curse_ast_lowering::Lowerer::new(&arena);
    let hir_program = curse_ast_lowering::Lower::lower(&ast_program, &mut lowerer);

    let (_, fun_def) = hir_program.function_defs.iter().next().unwrap();

    reset_sym_counter();
    compile_match_expr(fun_def.arms, gensym("x"), gensym("y"))
}

#[test]
fn basic_numbers2() {
    let input = r#"
        fn foo (
            |1| 0,
            |n| n,
        )
    "#;

    let arena = Bump::new();

    use Constructor::*;
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

    assert_eq!(get_decision(input, &arena), expected);
}

#[test]
fn basic_ctors() {
    let input = r#"
        fn foo (
            |Option::Some x| x,
            |Option::None {}| 0,
        )
    "#;

    let arena = Bump::new();
    let _interner = curse_interner::init();
    let path1 = &[idnt("Option"), idnt("Some")];
    let path2 = &[idnt("Option"), idnt("None")];

    use Constructor::*;
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
                        value: BindingValue::Record {
                            name: var("x__1_"),
                            index: 1,
                        },
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
                    constructor: NamedConstructor(path2, Box::new(Record(vec![]))),
                },
                match_path: Box::new(Branch {
                    test: Test {
                        variable: var("c__4_"),
                        constructor: Record(vec![]),
                    },
                    match_path: Box::new(Success(Body {
                        value: ExprKind::Lit(Lit::Integer(0)),
                        bindings: vec![Binding {
                            variable: var("c__4_"),
                            value: BindingValue::Record {
                                name: var("x__1_"),
                                index: 1,
                            },
                        }],
                    })),
                    fail_path: Box::new(Failure),
                }),
                fail_path: Box::new(Failure),
            }),
        }),
        fail_path: Box::new(Failure),
    };

    assert_eq!(get_decision(input, &arena), expected);
}

#[test]
fn basic_record() {
    let input = r#"
        fn foo (
            |{ a, b }, c| 3,
        )
    "#;
    let arena = Bump::new();

    use Constructor::*;
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

    assert_eq!(get_decision(input, &arena), expected);
}

#[test]
fn complex() {
    let input = r#"
        fn foo (
            |Add { x: 0, y: 0 }| 1,
            |Mul { x: 0, y }| 2,
            |Add { x: Succ x, y }| 3,
            |Mul { x, y: 0 }| 4,
            |Mul { x: Add { x, y }, y: z }| 5,
            |Add { x, y: 0 }| 6,
            |x| 7,
        )
    "#;
    let arena = Bump::new();

    // assert_eq!(get_decision(input, &arena), Failure);
}
