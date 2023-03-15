use crate::interpreter::{builtins::default_env, eval_expr};
use curse_ast::Context;
use curse_parse::{parse_expr, SourceErrors};
use miette::NamedSource;
use rustyline::error::ReadlineError;

pub fn repl() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;

    let handler = miette::GraphicalReportHandler::new();
    let mut buf = String::with_capacity(1024);

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line)?;

                let mut env = default_env();
                let arena = Context::new();

                let expr = parse_expr(&arena, &line).map_err(|e| SourceErrors {
                    source: NamedSource::new("test", line.to_string()),
                    errors: vec![e.into()],
                });

                let expr = match expr {
                    Ok(ast) => ast,
                    Err(err) => {
                        handler.render_report(&mut buf, &err).expect("fmt failed");
                        println!("{buf}");
                        buf.clear();
                        continue;
                    }
                };

                match eval_expr(expr, &mut env) {
                    Ok(result) => println!("{result}"),
                    Err(err) => {
                        println!("{err}");
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
