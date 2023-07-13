use crate::ast::{tok, Constructor, Lit, Record};
use crate::ast_struct;
use curse_span::{HasSpan, Span};

mod closure;
pub use closure::*;

#[derive(Clone, Debug)]
pub enum Expr<'ast, 'input> {
    Paren(Paren<'ast, 'input>),
    Symbol(Symbol),
    Lit(Lit<'input>),
    Record(Record<'input, ExprRef<'ast, 'input>>),
    Constructor(Constructor<'ast, 'input, Self>),
    Closure(&'ast Closure<'ast, 'input>),
    Appl(Appl<'ast, 'input>),
    Error,
}

pub type ExprRef<'ast, 'input> = &'ast Expr<'ast, 'input>;

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Paren<'ast, 'input> {
        pub lparen: tok::LParen,
        pub expr: &'ast Expr<'ast, 'input>,
        pub rparen: tok::RParen,
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Symbol {
    Plus(tok::Plus),
    Minus(tok::Minus),
    Star(tok::Star),
    Dot(tok::Dot),
    DotDot(tok::DotDot),
    Semi(tok::Semi),
    Percent(tok::Percent),
    Slash(tok::Slash),
    Eq(tok::Eq),
    Lt(tok::Lt),
    Gt(tok::Gt),
    Le(tok::Le),
    Ge(tok::Ge),
}

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Appl<'ast, 'input> {
        pub lhs: &'ast Expr<'ast, 'input>,
        pub fun: &'ast Expr<'ast, 'input>,
        pub rhs: &'ast Expr<'ast, 'input>,
    }
}

// === impl Span ===

impl HasSpan for Expr<'_, '_> {
    fn start(&self) -> u32 {
        match self {
            Expr::Paren(paren) => paren.start(),
            Expr::Symbol(symbol) => symbol.start(),
            Expr::Lit(lit) => lit.start(),
            Expr::Record(record) => record.start(),
            Expr::Constructor(constructor) => constructor.start(),
            Expr::Closure(closure) => closure.start(),
            Expr::Appl(appl) => appl.start(),
            Expr::Error => todo!(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Expr::Paren(paren) => paren.end(),
            Expr::Symbol(symbol) => symbol.end(),
            Expr::Lit(lit) => lit.end(),
            Expr::Record(record) => record.end(),
            Expr::Constructor(constructor) => constructor.end(),
            Expr::Closure(closure) => closure.end(),
            Expr::Appl(appl) => appl.end(),
            Expr::Error => todo!(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Expr::Paren(paren) => paren.span(),
            Expr::Symbol(symbol) => symbol.span(),
            Expr::Lit(lit) => lit.span(),
            Expr::Record(record) => record.span(),
            Expr::Constructor(constructor) => constructor.span(),
            Expr::Closure(closure) => closure.span(),
            Expr::Appl(appl) => appl.span(),
            Expr::Error => todo!(),
        }
    }
}

impl HasSpan for Paren<'_, '_> {
    fn start(&self) -> u32 {
        self.lparen.start()
    }

    fn end(&self) -> u32 {
        self.rparen.end()
    }
}

impl HasSpan for Symbol {
    fn start(&self) -> u32 {
        match self {
            Symbol::Plus(plus) => plus.start(),
            Symbol::Minus(minus) => minus.start(),
            Symbol::Star(star) => star.start(),
            Symbol::Dot(dot) => dot.start(),
            Symbol::DotDot(dotdot) => dotdot.start(),
            Symbol::Semi(semi) => semi.start(),
            Symbol::Percent(percent) => percent.start(),
            Symbol::Slash(slash) => slash.start(),
            Symbol::Eq(eq) => eq.start(),
            Symbol::Lt(lt) => lt.start(),
            Symbol::Gt(gt) => gt.start(),
            Symbol::Le(le) => le.start(),
            Symbol::Ge(ge) => ge.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Symbol::Plus(plus) => plus.end(),
            Symbol::Minus(minus) => minus.end(),
            Symbol::Star(star) => star.end(),
            Symbol::Dot(dot) => dot.end(),
            Symbol::DotDot(dotdot) => dotdot.end(),
            Symbol::Semi(semi) => semi.end(),
            Symbol::Percent(percent) => percent.end(),
            Symbol::Slash(slash) => slash.end(),
            Symbol::Eq(eq) => eq.end(),
            Symbol::Lt(lt) => lt.end(),
            Symbol::Gt(gt) => gt.end(),
            Symbol::Le(le) => le.end(),
            Symbol::Ge(ge) => ge.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Symbol::Plus(plus) => plus.span(),
            Symbol::Minus(minus) => minus.span(),
            Symbol::Star(star) => star.span(),
            Symbol::Dot(dot) => dot.span(),
            Symbol::DotDot(dotdot) => dotdot.span(),
            Symbol::Semi(semi) => semi.span(),
            Symbol::Percent(percent) => percent.span(),
            Symbol::Slash(slash) => slash.span(),
            Symbol::Eq(eq) => eq.span(),
            Symbol::Lt(lt) => lt.span(),
            Symbol::Gt(gt) => gt.span(),
            Symbol::Le(le) => le.span(),
            Symbol::Ge(ge) => ge.span(),
        }
    }
}

impl HasSpan for Appl<'_, '_> {
    fn start(&self) -> u32 {
        self.lhs.start()
    }

    fn end(&self) -> u32 {
        self.rhs.end()
    }
}
