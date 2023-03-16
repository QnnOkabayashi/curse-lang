use crate::interpreter::{builtins::default_env, eval_expr};
use curse_parse::{parse_expr, Context, SourceErrors};
use miette::{IntoDiagnostic, NamedSource};
use rustyline::error::ReadlineError;

pub fn repl() -> miette::Result<()> {
    let mut rl = rustyline::DefaultEditor::new().into_diagnostic()?;

    let handler = miette::GraphicalReportHandler::new();
    let mut buf = String::with_capacity(1024);

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line).into_diagnostic()?;

                let mut env = default_env();
                let arena = Context::new();

                let expr = parse_expr(&arena, &line).map_err(|errors| SourceErrors {
                    code: NamedSource::new("repl", line.to_string()),
                    errors,
                });

                let expr = match expr {
                    Ok(ast) => ast,
                    Err(err) => {
                        handler.render_report(&mut buf, &err).into_diagnostic()?;
                        println!("{buf}");
                        buf.clear();
                        continue;
                    }
                };

                match eval_expr(expr, &mut env) {
                    Ok(result) => println!("{result}"),
                    Err(err) => {
                        handler.render_report(&mut buf, &err).into_diagnostic()?;
                        println!("{buf}");
                        buf.clear();
                        continue;
                    }
                };
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
