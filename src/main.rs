use lalrpop_util::lalrpop_mod;
use miette::NamedSource;
use typed_arena::Arena;

lalrpop_mod!(pub curse1);
mod ast;
mod error;

// TODO:
// Arena allocator
// Spans on tokens
// Custom errors
// Top level items (fn, ...)
// Syntax for types
// Make it actually compile into an executable or be interpreted.

fn main() -> miette::Result<()> {
    let arena = Arena::with_capacity(1024);
    let input = _IN;
    let e = curse1::EndExprParser::new()
        .parse(&arena, input)
        .map_err(|e| {
            miette::Report::from(error::SourceErrors {
                source: NamedSource::new("test", input.to_string()),
                errors: vec![e.into()],
            })
        })?;

    println!("{e:#?}");
    Ok(())
}

const _IN: &str = r#"
(|x f| x f ()) (|in|
    4 in |x|
    5 in |y|
    x in print;
    x + y
) ()
"#;

const _ITER: &str = r#"
0 .. 20
    map (|x| x + 1)
    step_by 2
    collect ()
"#;
