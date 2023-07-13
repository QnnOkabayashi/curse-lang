use crate::ast::{tok, Expr, Pat, Type};
use crate::ast_struct;
use curse_span::{HasSpan, Span};

#[derive(Clone, Debug)]
pub enum Closure<'ast, 'input> {
    NonPiecewise(Arm<'ast, 'input>),
    Piecewise(
        tok::LParen,
        // Parser will only produce vecs with len >= 1
        Vec<(Arm<'ast, 'input>, tok::Comma)>,
        Option<Arm<'ast, 'input>>,
        tok::RParen,
    ),
    Empty(tok::LParen, tok::RParen),
}

impl<'ast, 'input> Closure<'ast, 'input> {
    pub fn arms(&self) -> impl Iterator<Item = &Arm<'ast, 'input>> {
        let (arms, last) = match self {
            Closure::NonPiecewise(arm) => (&[] as &[_], Some(arm)),
            Closure::Piecewise(_, arms, last, _) => (arms.as_slice(), last.as_ref()),
            Closure::Empty(_, _) => (&[] as &[_], None),
        };

        arms.iter().map(|(arm, _comma)| arm).chain(last)
    }
}

impl HasSpan for Closure<'_, '_> {
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
    pub struct Arm<'ast, 'input> {
        pub open: tok::Pipe,
        // There should only be up to 2 params,
        // but more shouldn't make the parser fail.
        pub params: Vec<(Param<'ast, 'input>, tok::Comma)>,
        pub last: Option<Param<'ast, 'input>>,
        pub close: tok::Pipe,
        pub body: &'ast Expr<'ast, 'input>,
    }
}

impl<'ast, 'input> Arm<'ast, 'input> {
    pub fn params_len(&self) -> usize {
        self.params.len() + self.last.is_some() as usize
    }
}

impl HasSpan for Arm<'_, '_> {
    fn start(&self) -> u32 {
        self.open.start()
    }

    fn end(&self) -> u32 {
        self.body.end()
    }
}

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Param<'ast, 'input> {
        pub pat: &'ast Pat<'ast, 'input>,
        pub ascription: Option<(tok::Colon, &'ast Type<'ast, 'input>)>,
    }
}
