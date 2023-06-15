use std::fmt;

pub struct Map<'hir, K, V> {
    pub entries: &'hir [(K, V)],
}

// Manual impl because `#[derive(Clone)]` has `T: Clone` bound.
impl<K, V> Clone for Map<'_, K, V> {
    fn clone(&self) -> Self {
        Map {
            entries: self.entries,
        }
    }
}

// Manual impl because `#[derive(Copy)]` has `T: Copy` bound.
impl<K, V> Copy for Map<'_, K, V> {}

impl<K, V> fmt::Debug for Map<'_, K, V>
where
    K: fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.entries.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}
