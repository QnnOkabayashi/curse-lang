use crate::ast::{ChoiceDef, FunctionDef, StructDef};

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub function_defs: Vec<FunctionDef>,
    pub struct_defs: Vec<StructDef>,
    pub choice_defs: Vec<ChoiceDef>,
}

impl Program {
    pub fn with_function_def(mut self, function_def: FunctionDef) -> Self {
        self.function_defs.push(function_def);
        self
    }

    pub fn with_struct_def(mut self, struct_def: StructDef) -> Self {
        self.struct_defs.push(struct_def);
        self
    }

    pub fn with_choice_def(mut self, choice_def: ChoiceDef) -> Self {
        self.choice_defs.push(choice_def);
        self
    }
}
