use crate::{tok, Span};
use std::fmt;

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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T> PatTuple<Option<T>> {
    pub fn fold(self) -> Option<PatTuple<T>> {
        let kind = self.kind.and_then(|nonempty| {
            Some(TupleNonempty {
                first: nonempty.first?,
                comma: nonempty.comma,
                remaining: nonempty
                    .remaining
                    .into_iter()
                    .map(|(t, comma)| t.map(|t| (t, comma)))
                    .collect::<Option<_>>()?,
                trailing: match nonempty.trailing {
                    Some(Some(t)) => Some(t),
                    Some(None) => return None,
                    None => None,
                },
            })
        });

        Some(PatTuple {
            lparen: self.lparen,
            kind,
            rparen: self.rparen,
        })
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