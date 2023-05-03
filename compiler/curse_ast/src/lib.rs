mod choice;
mod expr;
mod function;
mod pat;
pub mod tok;
mod ty;

pub use choice::*;
pub use expr::*;
pub use function::*;
pub use pat::*;
pub use ty::*;

pub trait Span {
    fn span(&self) -> (usize, usize);

    fn span_between(&self, other: impl Span) -> (usize, usize) {
        let (start1, len1) = self.span();
        let (start2, len2) = other.span();
        let start = std::cmp::min(start1, start2);
        let end = std::cmp::max(start1 + len1, start2 + len2);
        (start, end - start)
    }
}

impl<T: Span> Span for &T {
    fn span(&self) -> (usize, usize) {
        (*self).span()
    }
}

impl Span for (usize, usize) {
    fn span(&self) -> (usize, usize) {
        *self
    }
}

#[derive(Clone, Debug)]
pub struct Program<'ast, 'input> {
    pub fn_defs: Vec<FnDef<'ast, 'input>>,
    pub choice_defs: Vec<ChoiceDef<'ast, 'input>>,
}

impl<'ast, 'input> Program<'ast, 'input> {
    pub fn new() -> Self {
        Program {
            fn_defs: vec![],
            choice_defs: vec![],
        }
    }

    pub fn with_fn_def(mut self, fn_def: FnDef<'ast, 'input>) -> Self {
        self.fn_defs.push(fn_def);
        self
    }

    pub fn with_choice_def(mut self, choice_def: ChoiceDef<'ast, 'input>) -> Self {
        self.choice_defs.push(choice_def);
        self
    }
}
