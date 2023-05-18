use std::fmt;

#[derive(Copy, Clone, Debug, Default)]
pub struct Spanned<Kind> {
    pub kind: Kind,
    pub span: (usize, usize),
}

impl<Kind: fmt::Display> fmt::Display for Spanned<Kind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.kind, f)
    }
}
