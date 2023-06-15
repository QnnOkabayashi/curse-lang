use crate::{ast_struct, tok, Expr, Pat, Type};
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
