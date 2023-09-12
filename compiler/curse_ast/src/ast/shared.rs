use crate::{ast::tok, ast_struct};
use curse_interner::Ident;
use curse_span::{HasSpan, Span};
use std::mem;

ast_struct! {
    /// Hack to get around the fact that chaining a slice and
    /// one last element is no longer `ExactSizeIterator`.
    ///
    /// https://doc.rust-lang.org/std/iter/trait.ExactSizeIterator.html#when-shouldnt-an-adapter-be-exactsizeiterator
    #[derive(Debug)]
    pub struct Iter<'a, T, D> {
        parts: std::slice::Iter<'a, (T, D)>,
        last: Option<&'a T>,
    }
}

impl<'a, T, D> Iterator for Iter<'a, T, D> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.parts
            .next()
            .map(|(t, _d)| t)
            .or_else(|| self.last.take())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.last.is_none() {
            (0, Some(0))
        } else {
            let (lo, hi) = self.parts.size_hint();
            // hi <= isize::MAX since it comes from a slice,
            // which must point to an allocation, which cannot be
            // > isize::MAX
            // Therefore, it follows that hi + 1 < usize::MAX
            (lo + 1, hi.map(|hi| hi + 1))
        }
    }
}

impl<'a, T, D> ExactSizeIterator for Iter<'a, T, D> {}

#[derive(Copy, Clone, Debug)]
pub enum Lit {
    Integer(Ident),
    Ident(Ident),
    True(tok::True),
    False(tok::False),
}

ast_struct! {
    /// A path, e.g. `std::collections::Vec`
    #[derive(Clone, Debug)]
    pub struct Path {
        pub parts: Vec<(Ident, tok::ColonColon)>,
        pub ident: Ident,
    }
}

impl Path {
    pub fn iter_parts(&self) -> Iter<'_, Ident, tok::ColonColon> {
        Iter::new(self.parts.iter(), Some(&self.ident))
    }
}

/// A constructor, e.g. `Option::None {}`
#[derive(Clone, Debug)]
pub struct Constructor<T> {
    pub path: Path,
    pub inner: T,
}

impl<T> Constructor<T> {
    pub fn new(mut path: Path, variant: Option<(tok::ColonColon, Ident)>, inner: T) -> Self {
        if let Some((colon_colon, variant)) = variant {
            let last = mem::replace(&mut path.ident, variant);
            path.parts.push((last, colon_colon));
        }

        Constructor { path, inner }
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

impl<T: HasSpan> HasSpan for Constructor<T> {
    fn start(&self) -> u32 {
        self.path.start()
    }

    fn end(&self) -> u32 {
        self.inner.end()
    }
}
