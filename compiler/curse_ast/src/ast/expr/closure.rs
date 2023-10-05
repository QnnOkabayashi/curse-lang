use crate::ast::{tok, Expr, Pat, Type};
use crate::ast_struct;
use curse_span::{HasSpan, Span};

#[derive(Clone, Debug)]
pub enum Closure {
    NonPiecewise(Arm),
    Piecewise(tok::LParen, Vec<Arm>, tok::RParen),
}

impl Closure {
    pub fn arms(&self) -> &[Arm] {
        match self {
            Closure::NonPiecewise(arm) => std::slice::from_ref(arm),
            Closure::Piecewise(_, arms, _) => arms,
        }
    }
}

impl HasSpan for Closure {
    fn start(&self) -> u32 {
        match self {
            Closure::NonPiecewise(arm) => arm.start(),
            Closure::Piecewise(lparen, ..) => lparen.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Closure::NonPiecewise(arm) => arm.end(),
            Closure::Piecewise(.., rparen) => rparen.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Closure::NonPiecewise(arm) => arm.span(),
            Closure::Piecewise(lparen, _, rparen) => Span {
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
        pub params: Vec<Param>,
        pub close: tok::Pipe,
        pub body: Expr,
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
        pub ascription: Option<Type>,
    }
}

impl HasSpan for Param {
    fn start(&self) -> u32 {
        self.pat.start()
    }

    fn end(&self) -> u32 {
        if let Some(ascription) = self.ascription.as_ref() {
            ascription.end()
        } else {
            self.pat.end()
        }
    }
}
