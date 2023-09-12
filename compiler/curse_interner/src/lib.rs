#![forbid(unsafe_code)]

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

#[derive(Copy, Clone, Eq, Hash)]
pub struct Ident {
    pub symbol: InternedString,
    pub span: Span,
}

impl Ident {
    pub fn new(s: &str, span: Span) -> Self {
        Ident {
            symbol: InternedString::get_or_intern(s),
            span,
        }
    }

    pub fn new_in(s: &str, span: Span, interner: &mut StringInterner) -> Self {
        Ident {
            symbol: InternedString::get_or_intern_in(s, interner),
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
        fmt::Display::fmt(&self.symbol, f)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.symbol, f)
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

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.symbol.partial_cmp(&other.symbol)
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.symbol.cmp(&other.symbol)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedString(string_interner::DefaultSymbol);

impl InternedString {
    pub fn get_or_intern(s: &str) -> Self {
        Self::get_or_intern_in(
            s,
            STRINGS.write().as_mut().expect("no string interner loaded"),
        )
    }

    pub fn get(s: &str) -> Option<Self> {
        Self::get_in(
            s,
            STRINGS.read().as_ref().expect("no string interner loaded"),
        )
    }

    pub fn get_in(s: &str, interner: &StringInterner) -> Option<Self> {
        interner.get(s).map(InternedString)
    }

    pub fn get_or_intern_in(s: &str, interner: &mut StringInterner) -> Self {
        InternedString(interner.get_or_intern(s))
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
