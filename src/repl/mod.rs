use rustyline;
use rustyline::error::ReadlineError;

use miette::NamedSource;
use typed_arena::Arena;

use crate::{
    curse1, error,
    interpreter::{builtins::default_env, eval},
};

pub fn repl() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                let arena = Arena::with_capacity(1024);
                let mut env = default_env();

                let e = curse1::EndExprParser::new()
                    .parse(&arena, &line)
                    .map_err(|e| {
                        miette::Report::from(error::SourceErrors {
                            source: NamedSource::new("test", line.to_string()),
                            errors: vec![e.into()],
                        })
                    });

                let e = match e {
                    Ok(ast) => ast,
                    Err(err) => {
                        println!("{}", err);
                        continue;
                    }
                };

                let e = match eval(e, &mut env) {
                    Ok(ast) => ast,
                    Err(err) => {
                        println!("{err}");
                        continue;
                    }
                };

                println!("{e}");
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
