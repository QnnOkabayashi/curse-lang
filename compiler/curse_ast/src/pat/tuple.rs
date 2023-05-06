use crate::{tok, Res, Span};
use std::{fmt, iter};

#[derive(Clone, Debug)]
pub struct PatTuple<T> {
    pub lparen: tok::LParen,
    pub kind: Option<TupleNonempty<T>>,
    pub rparen: tok::RParen,
}

impl<T> Span for PatTuple<T> {
    fn span(&self) -> (usize, usize) {
        // Span::span_between(&self.lparen, self.rparen)
        self.lparen.span_between(self.rparen)
    }
}

#[derive(Clone, Debug)]
pub struct TupleNonempty<T> {
    pub first: T,
    pub comma: tok::Comma,
    pub remaining: Vec<(T, tok::Comma)>,
    pub trailing: Option<T>,
}

impl<T> TupleNonempty<Res<T>> {
    /// Converts a `TupleNonempty<Res<T>>` into a `Res<TupleNonempty<T>>`.
    pub fn transpose(self) -> Res<TupleNonempty<T>> {
        Ok(TupleNonempty {
            first: self.first?,
            comma: self.comma,
            remaining: self
                .remaining
                .into_iter()
                .map(|(t, comma)| t.map(|t| (t, comma)))
                .collect::<Res<_>>()?,
            trailing: self.trailing.transpose()?,
        })
    }
}

impl<T> PatTuple<T> {
    pub fn unit_from_grammar(lparen: tok::LParen, rparen: tok::RParen) -> Self {
        PatTuple {
            lparen,
            kind: None,
            rparen,
        }
    }

    pub fn nonempty_from_grammar(
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

    pub fn iter_elements(&self) -> impl Iterator<Item = &T> {
        self.kind
            .as_ref()
            .map(|inner| {
                iter::once(&inner.first)
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
