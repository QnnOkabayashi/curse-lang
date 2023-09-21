use std::{collections::HashSet, fs, io, mem};

use bumpalo::Bump;
use curse_ast::ast;
use curse_interner::{InternedString, StringInterner};

mod builtins;
mod error;
mod evaluation;
mod value;

/// Concats AST items (fn, struct, choice defs) into one list of items,
/// and deduplicates any imports.
struct ImportResolver<'intern> {
    interner: &'intern mut StringInterner,
    function_defs: Vec<ast::FunctionDef>,
    struct_defs: Vec<ast::StructDef>,
    choice_defs: Vec<ast::ChoiceDef>,
    imported: HashSet<InternedString>,
    errors: Vec<Error>,
}

#[derive(Debug)]
enum Error {
    Io(io::Error),
    Parse {
        file: InternedString,
        errors: Vec<curse_parse::Error>,
    },
}

impl<'intern> ImportResolver<'intern> {
    fn new(interner: &'intern mut StringInterner) -> Self {
        ImportResolver {
            interner,
            function_defs: vec![],
            struct_defs: vec![],
            choice_defs: vec![],
            imported: HashSet::new(),
            errors: vec![],
        }
    }

    /// The `file` parameter should be JUST the part before the `.curse`.
    /// So the file `std.curse` would be referred to as `std`.
    fn flatten_asts(&mut self, file: InternedString) {
        // We should reuse the buffer for formatting but I'm doing it
        // this way because it's dead simple and easy to understand.
        let input = match fs::read_to_string(&format!(
            "{}.curse",
            file.string_in(self.interner)
                .expect("ident not in interner")
        )) {
            Ok(input) => input,
            Err(err) => {
                self.errors.push(Error::Io(err));
                return;
            }
        };

        let mut parser = curse_parse::Parser::new(self.interner);

        let ast::Program {
            function_defs,
            struct_defs,
            choice_defs,
            dynamic_imports,
        } = parser.parse_program(&input);

        if !parser.errors.is_empty() {
            self.errors.push(Error::Parse {
                file,
                errors: mem::take(&mut parser.errors),
            });
            return;
        }

        self.function_defs.extend(function_defs);
        self.struct_defs.extend(struct_defs);
        self.choice_defs.extend(choice_defs);

        for import in dynamic_imports {
            let file = import.module.symbol;

            // Deduplicate by inserting into the set
            if self.imported.insert(file) {
                self.flatten_asts(file);
            }
        }
    }

    fn finish(self) -> Result<ast::Program, Vec<Error>> {
        if self.errors.is_empty() {
            Ok(ast::Program {
                function_defs: self.function_defs,
                struct_defs: self.struct_defs,
                choice_defs: self.choice_defs,
                dynamic_imports: vec![],
            })
        } else {
            Err(self.errors)
        }
    }
}

pub fn main() {
    let file = std::env::args()
        .skip(1)
        .next()
        .expect("usage: ./curse_interpreter <file without .curse ext>");

    let mut interner = StringInterner::new();

    let file = InternedString::get_or_intern_in(file.trim(), &mut interner);

    let mut import_resolver = ImportResolver::new(&mut interner);
    import_resolver.flatten_asts(file);
    let ast_program = match import_resolver.finish() {
        Ok(program) => program,
        Err(errors) => {
            curse_interner::replace(Some(interner));
            eprintln!("{errors:?}");
            return;
        }
    };

    curse_interner::replace(Some(interner));

    let hir_arena = Bump::new();
    let mut lowerer = curse_ast_lowering::Lowerer::new(&hir_arena);
    let hir_program = curse_ast_lowering::Lower::lower(&ast_program, &mut lowerer);

    if !lowerer.errors.is_empty() {
        eprintln!("{:?}", lowerer.errors);
        return;
    }

    match evaluation::execute_program(&hir_program) {
        Ok(val) => println!("{:#?}", &val),
        Err(e) => eprintln!("{e:?}"),
    }
}
