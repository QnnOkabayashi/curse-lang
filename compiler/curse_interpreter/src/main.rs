use std::io;

use bumpalo::Bump;
use curse_ast::ast;
use curse_interner::StringInterner;

mod builtins;
mod error;
mod evaluation;
mod value;

// TODO(william): better return value
fn flatten_asts(interner: &mut StringInterner, filepath: &str) -> io::Result<ast::Program> {
    let input = std::fs::read_to_string(filepath).expect("file read error");

    let mut parser = curse_parse::Parser::new(interner);

    let mut ast_program = parser.parse_program(&input);
    if !parser.errors.is_empty() {
        eprintln!("{:?}", parser.errors);
        return Err(io::Error::new(io::ErrorKind::Other, "errors!!!"));
    }

    for import in ast_program.dynamic_imports.iter() {
        let s: &str = import
            .file_string
            .symbol
            .string_in(interner)
            .expect("file name in interner");
        // trim off quotes
        let s = s[1..s.len() - 1].to_string();
        let other_program = flatten_asts(interner, &s)?;
        ast_program
            .function_defs
            .extend(other_program.function_defs);
        ast_program.choice_defs.extend(other_program.choice_defs);
        ast_program.struct_defs.extend(other_program.struct_defs);
    }
    Ok(ast_program)
}

pub fn main() -> io::Result<()> {
    let file_name = std::env::args()
        .skip(1)
        .next()
        .expect("usage: ./curse_interpreter <filename>");

    let mut interner = StringInterner::new();

    let ast_program = flatten_asts(&mut interner, &file_name)?;

    curse_interner::replace(Some(interner));

    let hir_arena = Bump::new();
    let mut lowerer = curse_ast_lowering::Lowerer::new(&hir_arena);
    let hir_program = curse_ast_lowering::Lower::lower(&ast_program, &mut lowerer);

    if !lowerer.errors.is_empty() {
        eprintln!("{:?}", lowerer.errors);
        return Ok(());
    }

    match evaluation::execute_program(&hir_program) {
        Ok(val) => println!("{:#?}", &val),
        Err(e) => eprintln!("{e:?}"),
    }

    Ok(())
}
