use rustyline;
use rustyline::error::ReadlineError;

use miette::NamedSource;

use crate::{
    ast::{self, TopLevel},
    curse1, error,
    interpreter::{builtins::default_env, eval_expr},
};

pub fn repl() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line)?;

                let mut env = default_env();
                let arena = ast::Arena::new();

                let top_level = curse1::TopLevelParser::new()
                    .parse(&arena, &line)
                    .map_err(|e| {
                        miette::Report::from(error::SourceErrors {
                            source: NamedSource::new("test", line.to_string()),
                            errors: vec![e.into()],
                        })
                    });

                let top_level = match top_level {
                    Ok(ast) => ast,
                    Err(err) => {
                        println!("{}", err);
                        continue;
                    }
                };

                match top_level {
                    TopLevel::Function(_name, _body) => {
                        // lots of horrible lifetimes issues lie here
                        todo!();
                    }
                    TopLevel::Expr(expr) => match eval_expr(expr, &mut env) {
                        Ok(result) => println!("{result}"),
                        Err(err) => {
                            println!("{err}");
                            continue;
                        }
                    },
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
