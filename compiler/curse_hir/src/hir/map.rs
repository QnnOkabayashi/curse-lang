use curse_interner::Ident;
use std::fmt;

pub enum FieldBinding<'hir> {
    Binding(Ident),
    Tree(&'hir [(Ident, Option<Self>)]),
}

impl fmt::Debug for FieldBinding<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binding(ident) => fmt::Debug::fmt(ident, f),
            Self::Tree(tree) => {
                let mut debug_set = f.debug_set();
                for (ident, binding) in tree.iter() {
                    if let Some(binding) = binding {
                        debug_set.entry(&format_args!("{ident:?}: {binding:?}"));
                    } else {
                        debug_set.entry(&format_args!("{ident:?}"));
                    }
                }
                debug_set.finish()
            }
        }
    }
}

pub struct Record<'hir, T> {
    pub entries: &'hir [Field<'hir, T>],
}

pub enum Field<'hir, T> {
    Shorthand(Ident),
    BindingAndValue {
        binding: FieldBinding<'hir>,
        value: T,
    },
}

impl<T> fmt::Debug for Field<'_, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Shorthand(ident) => fmt::Debug::fmt(ident, f),
            Self::BindingAndValue { binding, value } => {
                fmt::Debug::fmt(binding, f)?;
                write!(f, ": ")?;
                fmt::Debug::fmt(value, f)
            }
        }
    }
}

impl<'hir, T> Record<'hir, T> {
    pub fn new(entries: &'hir [Field<'hir, T>]) -> Self {
        Record { entries }
    }
}

impl<T> Default for Record<'_, T> {
    fn default() -> Self {
        Record::new(&[])
    }
}

// Manual impl because `#[derive(Clone)]` has `T: Clone` bound.
impl<T> Clone for Record<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

// Manual impl because `#[derive(Copy)]` has `T: Copy` bound.
impl<T> Copy for Record<'_, T> {}

impl<T> fmt::Debug for Record<'_, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.entries.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}
