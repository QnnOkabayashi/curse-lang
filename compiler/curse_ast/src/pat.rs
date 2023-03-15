use crate::tok;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Pat<'arena, Lit> {
    Lit(Lit),
    Tuple(PatTuple<&'arena Pat<'arena, Lit>>),
}

#[derive(Clone, Debug)]
pub struct PatTuple<T> {
    pub lparen: tok::LParen,
    pub kind: Option<TupleNonempty<T>>,
    pub rparen: tok::RParen,
}

#[derive(Clone, Debug)]
pub struct TupleNonempty<T> {
    pub first: T,
    pub comma: tok::Comma,
    pub remaining: Vec<(T, tok::Comma)>,
    pub trailing: Option<T>,
}

impl<T> PatTuple<T> {
    pub fn nonempty(
        lparen: tok::LParen,
        first: T,
        comma: tok::Comma,
        remaining: Vec<(T, tok::Comma)>,
        trailing: Option<T>,
        rparen: tok::RParen,
    ) -> Self {
        PatTuple {
            lparen,
            kind: Some(TupleNonempty {
                first,
                comma,
                remaining,
                trailing,
            }),
            rparen,
        }
    }

    pub fn empty(lparen: tok::LParen, rparen: tok::RParen) -> Self {
        PatTuple {
            lparen,
            kind: None,
            rparen,
        }
    }

    pub fn iter_elements(&self) -> impl Iterator<Item = &T> {
        self.kind
            .as_ref()
            .map(|inner| {
                Some(&inner.first)
                    .into_iter()
                    .chain(inner.remaining.iter().map(|(elem, _)| elem))
                    .chain(inner.trailing.as_ref())
            })
            .into_iter()
            .flatten()
    }

    pub fn len(&self) -> usize {
        match self.kind.as_ref() {
            Some(inner) => 1 + inner.remaining.len() + if inner.trailing.is_some() { 1 } else { 0 },
            None => 0,
        }
    }
}

impl<T: fmt::Display> fmt::Display for PatTuple<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        if let Some(inner) = self.kind.as_ref() {
            write!(f, "{}, ", inner.first)?;
            for (elem, _) in inner.remaining.iter() {
                write!(f, "{elem}, ")?;
            }
            if let Some(trailing) = inner.trailing.as_ref() {
                write!(f, "{trailing}")?;
            }
        }
        write!(f, ")")
    }
}
