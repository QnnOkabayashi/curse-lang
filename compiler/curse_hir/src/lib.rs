use std::collections::HashMap;

use curse_ast as ast;

pub struct Env<'ast, 'input> {
    // map from function identifiers to their information
    function_types: HashMap<&'input str, &'ast ast::Type<'ast, 'input>>,
}

impl<'ast, 'input> Env<'ast, 'input> {
    pub fn from_ast(program: ast::Program<'ast, 'input>) -> Env<'ast, 'input> {
        let mut function_types = HashMap::new();
        for item in program.items.iter() {
            match item {
                ast::Item::Function(function) => {
                    let name = function.name.literal;
                    let ty = function.typ;
                    function_types.insert(name, ty);
                }
            }
        }

        Env { function_types }
    }
}
