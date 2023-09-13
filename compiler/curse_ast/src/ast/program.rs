use crate::ast::{bikeshed, ChoiceDef, FunctionDef, StructDef};

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub function_defs: Vec<FunctionDef>,
    pub struct_defs: Vec<StructDef>,
    pub choice_defs: Vec<ChoiceDef>,
    pub dynamic_imports: Vec<bikeshed::DynamicImport>,
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

    pub fn with_dynamic_import(mut self, dynamic_import: bikeshed::DynamicImport) -> Self {
        self.dynamic_imports.push(dynamic_import);
        self
    }
}
