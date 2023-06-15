use crate::ctx;
use std::fmt;
use string_interner::DefaultSymbol;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ValueSymbol(pub DefaultSymbol);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeSymbol(pub DefaultSymbol);

#[derive(Copy, Clone, Debug)]
pub struct ValueIdent {
    pub symbol: ValueSymbol,
    pub location: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct TypeIdent {
    pub symbol: TypeSymbol,
    pub location: usize,
}

// TODO(quinn): These two impl blocks are literally identical,
// maybe I'll come up with something nicer later.

impl ValueIdent {
    pub fn display<'cx>(self, ctx: &'cx ctx::Global<'cx>) -> impl fmt::Display + 'cx {
        Displayable(move |f: &mut fmt::Formatter<'_>| {
            write!(
                f,
                "{}",
                ctx.string_interner.borrow().resolve(self.symbol.0).unwrap()
            )
        })
    }
}

impl TypeIdent {
    pub fn display<'cx>(self, ctx: &'cx ctx::Global<'cx>) -> impl fmt::Display + 'cx {
        Displayable(move |f: &mut fmt::Formatter<'_>| {
            write!(
                f,
                "{}",
                ctx.string_interner.borrow().resolve(self.symbol.0).unwrap()
            )
        })
    }
}

struct Displayable<F>(F);

impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Display for Displayable<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

