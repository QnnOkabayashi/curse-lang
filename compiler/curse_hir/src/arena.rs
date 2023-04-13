//! An index-based arena.

use std::hash::Hash;
use std::ops::{Index, IndexMut};
use std::{fmt, marker::PhantomData};

/// Pointer into an [`Arena`].
pub struct P<T> {
    index: u32,
    _type: PhantomData<fn() -> T>,
}

impl<T> P<T> {
    pub fn new(index: u32) -> Self {
        P {
            index,
            _type: PhantomData,
        }
    }

    pub fn index(&self) -> u32 {
        self.index
    }
}

impl<T> Copy for P<T> {}

impl<T> Clone for P<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> fmt::Debug for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("P")
            .field("index", &self.index)
            .field("type", &std::any::type_name::<T>())
            .finish()
    }
}

impl<T> PartialEq for P<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for P<T> {}

impl<T> PartialOrd for P<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl<T> Ord for P<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> Hash for P<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Arena<T> {
    elements: Vec<T>,
}

impl<T> Arena<T> {
    /// Create a new [`Arena`].
    pub fn new() -> Self {
        Arena {
            elements: Vec::new(),
        }
    }

    /// Push a value to the arena, returning the index that can be used to access
    /// the pushed value.
    pub fn push(&mut self, value: T) -> P<T> {
        let index = self.elements.len() as u32;
        self.elements.push(value);
        P {
            index,
            _type: PhantomData,
        }
    }
}

impl<T> From<Vec<T>> for Arena<T> {
    fn from(elements: Vec<T>) -> Self {
        Arena { elements }
    }
}

impl<T> Index<P<T>> for Arena<T> {
    type Output = T;

    fn index(&self, index: P<T>) -> &Self::Output {
        &self.elements[index.index as usize]
    }
}

impl<T> IndexMut<P<T>> for Arena<T> {
    fn index_mut(&mut self, index: P<T>) -> &mut Self::Output {
        &mut self.elements[index.index as usize]
    }
}
