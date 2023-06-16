use curse_span::{HasSpan, Span};
use parking_lot::{RwLock, RwLockReadGuard};
use std::{fmt, mem, ops};
pub use string_interner::StringInterner;

static STRINGS: RwLock<Option<StringInterner>> = RwLock::new(None);

/// Replaces the old string interner with a new one, if any.
pub fn replace(interner: Option<StringInterner>) -> Option<StringInterner> {
    mem::replace(&mut *STRINGS.write(), interner)
}

/// Initializes a fresh [`StringInterner`] into the global registry, returning the old one if present.
pub fn init() -> Option<StringInterner> {
    replace(Some(StringInterner::new()))
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident {
    pub string: InternedString,
    pub span: Span,
}

impl Ident {
    pub fn new(s: &str, span: Span) -> Self {
        Ident {
            string: InternedString::new(s),
            span,
        }
    }
}

impl<T: AsRef<str> + HasSpan> From<T> for Ident {
    fn from(value: T) -> Self {
        Ident::new(value.as_ref(), value.span())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.string, f)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.string, f)
    }
}

impl HasSpan for Ident {
    fn start(&self) -> u32 {
        self.span.start
    }

    fn end(&self) -> u32 {
        self.span.end
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedString(string_interner::DefaultSymbol);

impl InternedString {
    pub fn new(s: &str) -> Self {
        InternedString(
            STRINGS
                .write()
                .as_mut()
                .expect("no string interner loaded")
                .get_or_intern(s),
        )
    }

    pub fn string(&self) -> StringGuard<'_> {
        StringGuard {
            guard: STRINGS.read(),
            symbol: *self,
        }
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&*self.string(), f)
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&*self.string(), f)
    }
}

pub struct StringGuard<'a> {
    guard: RwLockReadGuard<'a, Option<StringInterner>>,
    symbol: InternedString,
}

impl<'a> ops::Deref for StringGuard<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.guard
            .as_ref()
            .expect("no string interner loaded")
            .resolve(self.symbol.0)
            .expect("symbol not found in interner")
    }
}
