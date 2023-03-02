use lalrpop_util::lalrpop_mod;
use miette::NamedSource;

lalrpop_mod!(pub curse1);
mod ast;
mod error;

// TODO:
// Arena allocator
// Custom errors
// Top level items (fn, ...)
// Syntax for types
// Make it actually compile into an executable or be interpreted.

fn main() -> miette::Result<()> {
    let e = parse(_IN)?;

    println!("{e:#?}");
    Ok(())
}

fn parse(input: &str) -> miette::Result<ast::Expr<'_>> {
    curse1::EndExprParser::new().parse(input).map_err(|e| {
        error::SourceErrors {
            source: NamedSource::new("test", input.to_string()),
            errors: vec![e.into()],
        }.into()
    })
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
