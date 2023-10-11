use crate::{ast::tok, ast_struct};
use curse_interner::Ident;
use curse_span::{HasSpan, Span};

#[derive(Copy, Clone, Debug)]
pub enum Lit {
    Integer(Ident),
    Ident(Ident),
    True(tok::True),
    False(tok::False),
}

ast_struct! {
    /// A path, e.g. `std::collections`
    #[derive(Clone, Debug)]
    pub struct Path {
        pub parts: Vec<Ident>,
        pub last: Ident,
    }
}

ast_struct! {
    /// A constructor, e.g. `Option::None {}`
    #[derive(Clone, Debug)]
    pub struct Constructor<T> {
        pub ty: Ident,
        pub variant: Ident,
        pub inner: T,
    }
}

impl HasSpan for Lit {
    fn start(&self) -> u32 {
        match self {
            Lit::Integer(integer) => integer.start(),
            Lit::Ident(ident) => ident.start(),
            Lit::True(true_lit) => true_lit.start(),
            Lit::False(false_lit) => false_lit.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Lit::Integer(integer) => integer.end(),
            Lit::Ident(ident) => ident.end(),
            Lit::True(true_lit) => true_lit.end(),
            Lit::False(false_lit) => false_lit.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Lit::Integer(integer) => integer.span(),
            Lit::Ident(ident) => ident.span(),
            Lit::True(true_lit) => true_lit.span(),
            Lit::False(false_lit) => false_lit.span(),
        }
    }
}

impl HasSpan for Path {
    fn start(&self) -> u32 {
        self.parts.first().unwrap_or(&self.last).start()
    }

    fn end(&self) -> u32 {
        self.parts.last().unwrap_or(&self.last).end()
    }
}

impl<T: HasSpan> HasSpan for Constructor<T> {
    fn start(&self) -> u32 {
        self.ty.start()
    }

    fn end(&self) -> u32 {
        self.inner.end()
    }
}
