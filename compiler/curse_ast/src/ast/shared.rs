use crate::ast::tok;
use curse_span::{HasSpan, Span};
use std::mem;

#[derive(Copy, Clone, Debug)]
pub enum Lit<'input> {
    Integer(tok::Integer<'input>),
    Ident(tok::Ident<'input>),
    True(tok::True),
    False(tok::False),
}

/// A path, e.g. `std::collections::Vec`
#[derive(Clone, Debug)]
pub struct Path<'input> {
    pub parts: Vec<(tok::Ident<'input>, tok::ColonColon)>,
    pub ident: tok::Ident<'input>,
}

impl<'input> Path<'input> {
    pub fn new(
        parts: Vec<(tok::Ident<'input>, tok::ColonColon)>,
        ident: tok::Ident<'input>,
    ) -> Self {
        Path { parts, ident }
    }

    pub fn len(&self) -> usize {
        self.parts.len() + 1
    }
}

/// A constructor, e.g. `Option::None {}`
#[derive(Clone, Debug)]
pub struct Constructor<'ast, 'input, T> {
    pub path: Path<'input>,
    pub inner: &'ast T,
}

impl<'ast, 'input, T> Constructor<'ast, 'input, T> {
    pub fn new(
        mut path: Path<'input>,
        variant: Option<(tok::ColonColon, tok::Ident<'input>)>,
        inner: &'ast T,
    ) -> Self {
        if let Some((colon_colon, variant)) = variant {
            let last = mem::replace(&mut path.ident, variant);
            path.parts.push((last, colon_colon));
        }

        Constructor { path, inner }
    }
}

impl HasSpan for Lit<'_> {
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

impl HasSpan for Path<'_> {
    fn start(&self) -> u32 {
        if let Some((ident, _)) = self.parts.first() {
            ident.start()
        } else {
            self.ident.start()
        }
    }

    fn end(&self) -> u32 {
        self.ident.end()
    }
}

impl<T: HasSpan> HasSpan for Constructor<'_, '_, T> {
    fn start(&self) -> u32 {
        self.path.start()
    }

    fn end(&self) -> u32 {
        self.inner.end()
    }
}
