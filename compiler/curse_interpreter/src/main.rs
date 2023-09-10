use std::io;

use bumpalo::Bump;

mod builtins;
mod error;
mod evaluation;
mod value;

pub fn main() -> io::Result<()> {
    curse_interner::init();

    let ast_arena = Bump::new();
    let strings = Bump::new();
    let hir_arena = Bump::new();
    let mut parser = curse_parse::Parser::new(&ast_arena, &strings);

    let mut args = std::env::args();
    args.next();

    let file_name = args.next().expect("usage: ./curse_interpreter <filename>");
    let file = std::fs::read_to_string(file_name).expect("file read error");

    let ast_program = parser.parse_program(&file);
    if !parser.errors.is_empty() {
        eprintln!("{:?}", parser.errors);
        return Ok(());
    }

    let mut lowerer = curse_ast_lowering::Lowerer::new(&hir_arena);
    let hir_program = curse_ast_lowering::Lower::lower(&ast_program, &mut lowerer);

    if !lowerer.errors.is_empty() {
        eprintln!("{:?}", parser.errors);
        return Ok(());
    }

    match evaluation::execute_program(&hir_program) {
        Ok(val) => println!("{}", &val),
        Err(e) => eprintln!("{e:?}"),
    }

    Ok(())
}
