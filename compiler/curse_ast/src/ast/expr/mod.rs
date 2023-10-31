use crate::ast::{tok, Constructor, Lit, Pat, Record};
use curse_span::{HasSpan, Span};
use derive_more::From;

mod closure;
pub use closure::*;

#[derive(Clone, Debug)]
pub enum Expr {
    Paren(Box<Paren>),
    Symbol(Symbol),
    Lit(Lit),
    Record(Box<Record<Self>>),
    Constructor(Box<Constructor<Self>>),
    Closure(Box<Closure>),
    Appl(Box<Appl>),
    Region(Box<Region>),
    Error,
}

#[derive(Clone, Debug, From)]
pub struct Paren {
    pub lparen: tok::LParen,
    pub expr: Expr,
    pub rparen: tok::RParen,
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

#[derive(Clone, Debug, From)]
pub struct Appl {
    pub lhs: Expr,
    pub fun: Expr,
    pub rhs: Expr,
}

/// A region, e.g. `ref mut x { x + 1 }`
#[derive(Clone, Debug, From)]
pub struct Region {
    pub kind: RegionKind,
    // Should only be an ident or a record of idents with no values, e.g. `{ a, b }`.
    // Anything else will be caught at lowering.
    pub pat: Pat,
    pub lbrace: tok::LBrace,
    pub body: Expr,
    pub rbrace: tok::RBrace,
}

#[derive(Clone, Debug)]
pub enum RegionKind {
    Ref(tok::Ref),
    Mut(tok::Mut),
    RefMut(tok::Ref, tok::Mut),
}

// === impl Span ===

impl HasSpan for Expr {
    fn start(&self) -> u32 {
        match self {
            Expr::Paren(paren) => paren.start(),
            Expr::Symbol(symbol) => symbol.start(),
            Expr::Lit(lit) => lit.start(),
            Expr::Record(record) => record.start(),
            Expr::Constructor(constructor) => constructor.start(),
            Expr::Closure(closure) => closure.start(),
            Expr::Appl(appl) => appl.start(),
            Expr::Region(region) => region.start(),
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
            Expr::Region(region) => region.end(),
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
            Expr::Region(region) => region.span(),
            Expr::Error => todo!(),
        }
    }
}

impl HasSpan for Paren {
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

impl HasSpan for Appl {
    fn start(&self) -> u32 {
        self.lhs.start()
    }

    fn end(&self) -> u32 {
        self.rhs.end()
    }
}

impl HasSpan for Region {
    fn start(&self) -> u32 {
        self.kind.start()
    }

    fn end(&self) -> u32 {
        self.body.end()
    }
}

impl HasSpan for RegionKind {
    fn start(&self) -> u32 {
        match self {
            RegionKind::Ref(r#ref) => r#ref.start(),
            RegionKind::Mut(r#mut) => r#mut.start(),
            RegionKind::RefMut(r#ref, _) => r#ref.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            RegionKind::Ref(r#ref) => r#ref.end(),
            RegionKind::Mut(r#mut) => r#mut.end(),
            RegionKind::RefMut(_, r#mut) => r#mut.end(),
        }
    }

    fn span(&self) -> Span {
        match self {
            RegionKind::Ref(r#ref) => r#ref.span(),
            RegionKind::Mut(r#mut) => r#mut.span(),
            RegionKind::RefMut(r#ref, r#mut) => Span {
                start: r#ref.start(),
                end: r#mut.end(),
            },
        }
    }
}
