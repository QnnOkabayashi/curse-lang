use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub curse1);
mod ast;
mod error;
// mod interpreter;
// mod repl;

// TODO:
// support parsing piecewise closures (with curly braces)
// fix interpreter
// Spans on tokens
// Custom errors
// Top level items (fn, ...)
// Syntax for types

fn main() -> miette::Result<()> {
    use miette::IntoDiagnostic;

    let arena = typed_arena::Arena::with_capacity(1024);
    let e = curse1::EndExprParser::new()
        .parse(&arena, _REF)
        .into_diagnostic()?;
    println!("{e:#?}");

    // repl::repl().unwrap();
    Ok(())
}

const _MATH: &str = r#"
1 + 2 * 3 - 3
"#;

const _FUNC: &str = r#"
1 (|x y| x + y) 2
"#;

const _LET: &str = r#"
3 in |x| x in print
"#;

const _IN: &str = r#"
(|x f| x f ()) (|in|
    4 in |x|
    5 in |y|
    x + y
) ()
"#;

const _ITER: &str = r#"
0 .. 20
    map (|x| x + 1)
    step_by 2
    collect ()
"#;

const _PATS: &str = r#"
(1 * 2, 2 in |x| x * 2) (|(a, b) (c, d)|
    (a * c) + (b * d)
) (3, 4)
"#;

const _REF: &str = r#"
3 in {
    |1| 1 in print,
    |2| 2 in print,
    |3| 7 in print,
    |x| x in print,
}
"#;
