use lalrpop_util::lalrpop_mod;
use miette::NamedSource;
use typed_arena::Arena;

use crate::interpreter::{eval, builtins::default_env};

lalrpop_mod!(pub curse1);
mod ast;
mod error;
mod interpreter;

// TODO:
// Spans on tokens
// Custom errors
// Top level items (fn, ...)
// Syntax for types
// Make it actually compile into an executable or be interpreted.

fn main() -> miette::Result<()> {
    let arena = Arena::with_capacity(1024);
    let input = _LET;
    let mut env = default_env();
    let e = curse1::EndExprParser::new()
        .parse(&arena, input)
        .map_err(|e| {
            miette::Report::from(error::SourceErrors {
                source: NamedSource::new("test", input.to_string()),
                errors: vec![e.into()],
            })
        })?;

    println!("{e:#?}");
    println!("{}", eval(e, &mut env).unwrap());
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

    // x in print;
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
(1, 2) (|(a, b) (c, d)|
    (a * c) + (b * d)
) (3, 4)
"#;
