use curse_ast::Context;
use curse_parse::{parse_program, SourceErrors};
use miette::{IntoDiagnostic, NamedSource};
use std::{fs, path::PathBuf};

mod interpreter;
mod repl;

use clap::Parser;

// TODO:
// Custom errors
// Syntax for types

#[derive(Parser)]
struct Cli {
    /// The file to evaluate
    #[arg(long = "file")]
    file: Option<PathBuf>,
}

fn main() -> miette::Result<()> {
    if let Some(path) = Cli::parse().file {
        // Example: cargo run -- --file examples/branching.curse
        let input = fs::read_to_string(&path).into_diagnostic()?;
        let arena = Context::new();

        match parse_program(&arena, &input) {
            Ok(program) => interpreter::eval_program(program).unwrap(),
            Err(e) => {
                return Err(miette::Report::from(SourceErrors {
                    source: NamedSource::new(path.to_string_lossy(), input.clone()),
                    errors: vec![e.into()],
                }));
            }
        }
    } else {
        repl::repl().unwrap();
    }
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
