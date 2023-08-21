use crate::ast::{ChoiceDef, FunctionDef, StructDef};

#[derive(Clone, Debug, Default)]
pub struct Program<'ast> {
    pub function_defs: Vec<FunctionDef<'ast>>,
    pub struct_defs: Vec<StructDef<'ast>>,
    pub choice_defs: Vec<ChoiceDef<'ast>>,
}

impl<'ast> Program<'ast> {
    pub fn with_function_def(mut self, function_def: FunctionDef<'ast>) -> Self {
        self.function_defs.push(function_def);
        self
    }

    pub fn with_struct_def(mut self, struct_def: StructDef<'ast>) -> Self {
        self.struct_defs.push(struct_def);
        self
    }

    pub fn with_choice_def(mut self, choice_def: ChoiceDef<'ast>) -> Self {
        self.choice_defs.push(choice_def);
        self
    }
}
