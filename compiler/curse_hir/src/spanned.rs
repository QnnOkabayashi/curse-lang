use std::fmt;

/// A [`SourceSpan`] attached to some `T`.
#[derive(Copy, Clone)]
pub struct S<T>(pub T, pub (usize, usize));

impl<T> S<T> {
    pub fn new(span: (usize, usize), item: T) -> Self {
        S(item, span)
    }

    pub fn value(&self) -> &T {
        &self.0
    }

    pub fn span(&self) -> (usize, usize) {
        self.1
    }
}

impl<T: fmt::Display> fmt::Display for S<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.value(), f)
    }
}

impl<T: fmt::Debug> fmt::Debug for S<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.value(), f)
    }
}

pub trait WithSpan: Sized {
    fn with_span(self, span: (usize, usize)) -> S<Self>;
}

impl<T> WithSpan for T {
    fn with_span(self, span: (usize, usize)) -> S<Self> {
        S::new(span, self)
    }
}
