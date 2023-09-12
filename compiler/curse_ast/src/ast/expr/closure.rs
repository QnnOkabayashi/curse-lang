use crate::ast::{tok, Expr, Iter, Pat, Type};
use crate::ast_struct;
use curse_span::{HasSpan, Span};

#[derive(Clone, Debug)]
pub enum Closure {
    NonPiecewise(Arm),
    Piecewise(
        tok::LParen,
        // Parser will only produce vecs with len >= 1
        Vec<(Arm, tok::Comma)>,
        Option<Arm>,
        tok::RParen,
    ),
    Empty(tok::LParen, tok::RParen),
}

impl Closure {
    pub fn iter_arms(&self) -> Iter<'_, Arm, tok::Comma> {
        let (arms, last) = match self {
            Closure::NonPiecewise(arm) => (&[] as _, Some(arm)),
            Closure::Piecewise(_, arms, last, _) => (arms.as_slice(), last.as_ref()),
            Closure::Empty(_, _) => (&[] as _, None),
        };

        Iter::new(arms.iter(), last)
    }
}

impl HasSpan for Closure {
    fn start(&self) -> u32 {
        match self {
            Closure::NonPiecewise(arm) => arm.start(),
            Closure::Piecewise(lparen, ..) | Closure::Empty(lparen, ..) => lparen.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Closure::NonPiecewise(arm) => arm.end(),
            Closure::Piecewise(.., rparen) | Closure::Empty(_, rparen) => rparen.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Closure::NonPiecewise(arm) => arm.span(),
            Closure::Piecewise(lparen, .., rparen) | Closure::Empty(lparen, rparen) => Span {
                start: lparen.start(),
                end: rparen.end(),
            },
        }
    }
}

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Arm {
        pub open: tok::Pipe,
        // There should only be up to 2 params,
        // but more shouldn't make the parser fail.
        pub params: Vec<(Param, tok::Comma)>,
        pub last: Option<Param>,
        pub close: tok::Pipe,
        pub body: Expr,
    }
}

impl Arm {
    pub fn iter_params(&self) -> Iter<'_, Param, tok::Comma> {
        Iter::new(self.params.iter(), self.last.as_ref())
    }
}

impl HasSpan for Arm {
    fn start(&self) -> u32 {
        self.open.start()
    }

    fn end(&self) -> u32 {
        self.body.end()
    }
}

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Param {
        pub pat: Pat,
        pub ascription: Option<(tok::Colon, Type)>,
    }
}
