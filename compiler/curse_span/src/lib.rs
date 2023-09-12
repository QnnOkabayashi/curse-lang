#![forbid(unsafe_code)]

use core::fmt;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn start_len(self) -> (usize, usize) {
        (self.start as usize, (self.end - self.start) as usize)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, {})", self.start, self.end)
    }
}

pub trait HasSpan {
    /// The location of the first byte.
    fn start(&self) -> u32;

    /// The location of just past the last byte.
    fn end(&self) -> u32;

    /// A pair between start and end.
    ///
    /// Note that this function is implemented by default, but can be overriden
    /// to be slightly more efficient in the case that the implementor is an enum
    /// to avoid matching twice.
    fn span(&self) -> Span {
        Span {
            start: self.start(),
            end: self.end(),
        }
    }
}

impl HasSpan for Span {
    fn start(&self) -> u32 {
        self.start
    }

    fn end(&self) -> u32 {
        self.end
    }

    fn span(&self) -> Span {
        *self
    }
}

impl<T: HasSpan> HasSpan for &T {
    fn start(&self) -> u32 {
        (*self).start()
    }

    fn end(&self) -> u32 {
        (*self).end()
    }
}
