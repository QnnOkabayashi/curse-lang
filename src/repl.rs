use rustyline::error::ReadlineError;

use miette::NamedSource;

use crate::{
    ast, curse1, error,
    interpreter::{builtins::default_env, eval_expr},
    lex::Lexer,
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
                let lexer = Lexer::new(&line);

                let expr = curse1::EndExprParser::new()
                    .parse(&arena, lexer)
                    .map_err(|e| {
                        miette::Report::from(error::SourceErrors {
                            source: NamedSource::new("test", line.to_string()),
                            errors: vec![e.into()],
                        })
                    });

                let expr = match expr {
                    Ok(ast) => ast,
                    Err(err) => {
                        println!("{}", err);
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
