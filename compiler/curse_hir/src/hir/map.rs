use curse_interner::Ident;
use std::fmt;

/// The part of the field that binds identifiers.
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

/// A field of a record, either the shorthand syntax or a traditional binding-value pair.
pub enum FieldSyntax<'hir, T> {
    Shorthand(Ident),
    BindingAndValue(BindingAndValue<'hir, T>),
}

impl<T> fmt::Debug for FieldSyntax<'_, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Shorthand(ident) => fmt::Debug::fmt(ident, f),
            Self::BindingAndValue(binding_and_value) => fmt::Debug::fmt(binding_and_value, f),
        }
    }
}

/// Just binding and a value.
pub struct BindingAndValue<'hir, T> {
    pub binding: FieldBinding<'hir>,
    pub value: T,
}

impl<T> fmt::Debug for BindingAndValue<'_, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.binding, f)?;
        write!(f, ": ")?;
        fmt::Debug::fmt(&self.value, f)
    }
}
