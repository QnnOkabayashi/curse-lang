use crate::ast::{ChoiceDef, FunctionDef, StructDef};

#[derive(Clone, Debug, Default)]
pub struct Program<'ast, 'input> {
    pub function_defs: Vec<FunctionDef<'ast, 'input>>,
    pub struct_defs: Vec<StructDef<'ast, 'input>>,
    pub choice_defs: Vec<ChoiceDef<'ast, 'input>>,
}

impl<'ast, 'input> Program<'ast, 'input> {
    pub fn with_function_def(mut self, function_def: FunctionDef<'ast, 'input>) -> Self {
        self.function_defs.push(function_def);
        self
    }

    pub fn with_struct_def(mut self, struct_def: StructDef<'ast, 'input>) -> Self {
        self.struct_defs.push(struct_def);
        self
    }

    pub fn with_choice_def(mut self, choice_def: ChoiceDef<'ast, 'input>) -> Self {
        self.choice_defs.push(choice_def);
        self
    }
}
