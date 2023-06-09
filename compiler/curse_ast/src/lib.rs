mod def;
mod expr;
mod pat;
mod record;
pub mod tok;
mod ty;

pub use def::{
    ChoiceDef, ExplicitTypes, FunctionDef, GenericParams, StructDef, VariantDef, Variants,
};
pub use expr::{
    Appl, Arm, Closure, Constructor, Expr, FieldExpr, Lit, Param, Paren, RecordExpr, Symbol,
};
pub use pat::{FieldPat, Pat, RecordPat};
pub use record::Record;
pub use ty::{FieldType, GenericArgs, NamedType, RecordType, Type};

pub trait Span {
    /// The location of the first byte.
    fn start(&self) -> usize;

    /// The location of just past the last byte.
    fn end(&self) -> usize;

    /// A pair between start and end.
    ///
    /// Note that this function is implemented by default, but can be overriden
    /// to be slightly more efficient in the case that the implementor is an enum
    /// to avoid matching twice.
    fn span(&self) -> (usize, usize) {
        (self.start(), self.end())
    }
}

impl<T: Span> Span for &T {
    fn start(&self) -> usize {
        (*self).start()
    }

    fn end(&self) -> usize {
        (*self).end()
    }
}

impl Span for (usize, usize) {
    fn start(&self) -> usize {
        self.0
    }

    fn end(&self) -> usize {
        self.1
    }
}

#[derive(Clone, Debug)]
pub struct Program<'ast, 'input> {
    pub functions: Vec<FunctionDef<'ast, 'input>>,
    pub structs: Vec<StructDef<'ast, 'input>>,
    pub choices: Vec<ChoiceDef<'ast, 'input>>,
}

impl<'ast, 'input> Program<'ast, 'input> {
    pub fn new() -> Self {
        Program {
            functions: vec![],
            structs: vec![],
            choices: vec![],
        }
    }

    pub fn with_function_def(mut self, function_def: FunctionDef<'ast, 'input>) -> Self {
        self.functions.push(function_def);
        self
    }

    pub fn with_struct_def(mut self, struct_def: StructDef<'ast, 'input>) -> Self {
        self.structs.push(struct_def);
        self
    }

    pub fn with_choice_def(mut self, choice_def: ChoiceDef<'ast, 'input>) -> Self {
        self.choices.push(choice_def);
        self
    }
}
