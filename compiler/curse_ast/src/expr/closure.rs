use crate::{Expr, Pat, tok, Type, Span};

#[derive(Clone, Debug)]
pub enum Closure<'ast, 'input> {
    NonPiecewise(Arm<'ast, 'input>),
    Piecewise {
        lparen: tok::LParen,
        arms: Vec<(Arm<'ast, 'input>, tok::Comma)>,
        rparen: tok::RParen,
    },
}

impl<'ast, 'input> Closure<'ast, 'input> {
    pub fn arm_count(&self) -> usize {
        match self {
            Closure::NonPiecewise(_) => 1,
            Closure::Piecewise { arms, .. } => arms.len(),
        }
    }
}

impl Span for Closure<'_, '_> {
    fn start(&self) -> usize {
        match self {
            Closure::NonPiecewise(arm) => arm.start(),
            Closure::Piecewise { lparen, .. } => lparen.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Closure::NonPiecewise(arm) => arm.end(),
            Closure::Piecewise { rparen, .. } => rparen.end(),
        }
    }

    fn span(&self) -> (usize, usize) {
        match self {
            Closure::NonPiecewise(arm) => arm.span(),
            Closure::Piecewise { lparen, rparen, .. } => (lparen.start(), rparen.end()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Arm<'ast, 'input> {
    pub open: tok::Pipe,
    // There should only be up to 2 params,
    // but more shouldn't make the parser fail.
    pub params: Vec<(Param<'ast, 'input>, tok::Comma)>,
    pub trailing: Option<Param<'ast, 'input>>,
    pub close: tok::Pipe,
    pub body: &'ast Expr<'ast, 'input>,
}

impl Span for Arm<'_, '_> {
    fn start(&self) -> usize {
        self.open.start()
    }

    fn end(&self) -> usize {
        self.body.end()
    }
}

#[derive(Clone, Debug)]
pub struct Param<'ast, 'input> {
    pub pat: &'ast Pat<'ast, 'input>,
    pub ascription: Option<(tok::Colon, &'ast Type<'ast, 'input>)>,
}
