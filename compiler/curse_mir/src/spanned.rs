use curse_ast::Span;
use std::fmt;

#[derive(Copy, Clone, Default)]
pub struct Spanned<Kind> {
    pub kind: Kind,
    pub span: (usize, usize),
}

impl<Kind: fmt::Debug> fmt::Debug for Spanned<Kind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.kind, f)
    }
}

impl<Kind: fmt::Display> fmt::Display for Spanned<Kind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.kind, f)
    }
}

impl<Kind> Span for Spanned<Kind> {
    fn start(&self) -> usize {
        self.span.0
    }

    fn end(&self) -> usize {
        self.span.1
    }
}
