use curse_interner::Ident;
use std::fmt;

pub struct Map<'hir, T> {
    pub entries: &'hir [(Ident, T)],
}

impl<'hir, T> Map<'hir, T> {
    pub fn new(entries: &'hir [(Ident, T)]) -> Self {
        Map { entries }
    }
}

impl<T> Default for Map<'_, T> {
    fn default() -> Self {
        Map::new(&[])
    }
}

// Manual impl because `#[derive(Clone)]` has `T: Clone` bound.
impl<T> Clone for Map<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

// Manual impl because `#[derive(Copy)]` has `T: Copy` bound.
impl<T> Copy for Map<'_, T> {}

impl<T: fmt::Debug> fmt::Debug for Map<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.entries.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}
