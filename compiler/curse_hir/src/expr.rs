use crate::{Type, TypeFunction};

pub trait Ty<'hir> {
    fn ty(&'hir self) -> &'hir Type<'hir>;
}

#[derive(Debug, Clone)]
pub enum Expr<'hir, 'input> {
    Builtin(Builtin),
    Bool(bool),
    I32(i32),
    Ident(ExprIdent<'hir, 'input>),
    Tuple(ExprTuple<'hir, Expr<'hir, 'input>>),
    Closure(ExprClosure<'hir, 'input>),
    Appl(ExprAppl<'hir, 'input>),
}

impl<'hir> Ty<'hir> for Expr<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        match self {
            Expr::Builtin(builtin) => builtin.ty(),
            Expr::Bool(_) => &Type::Bool,
            Expr::I32(_) => &Type::I32,
            Expr::Tuple(tuple) => tuple.ty(),
            Expr::Ident(ident) => ident.ty(),
            Expr::Closure(closure) => closure.ty(),
            Expr::Appl(appl) => appl.ty(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Rem,
    Div,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Print,
}

impl<'hir> Ty<'hir> for Builtin {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        match self {
            Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Rem => {
                &Type::Function(TypeFunction {
                    lhs: &Type::I32,
                    rhs: &Type::I32,
                    output: &Type::I32,
                })
            }
            Builtin::Div => todo!("Type of div"),
            Builtin::Eq | Builtin::Lt | Builtin::Gt | Builtin::Le | Builtin::Ge => {
                &Type::Function(TypeFunction {
                    lhs: &Type::I32,
                    rhs: &Type::I32,
                    output: &Type::Bool,
                })
            }
            Builtin::Print => todo!("Type of print"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprIdent<'hir, 'input> {
    pub literal: &'input str,
    pub ty: &'hir Type<'hir>,
}

impl<'hir> Ty<'hir> for ExprIdent<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct ExprTuple<'hir, T> {
    pub exprs: Vec<&'hir T>,
    pub ty: &'hir Type<'hir>,
}

impl<'hir, T> Ty<'hir> for ExprTuple<'hir, T> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct ExprClosure<'hir, 'input> {
    pub ty: &'hir Type<'hir>,
    pub head: ExprBranch<'hir, 'input>,
    pub tail: Vec<ExprBranch<'hir, 'input>>,
}

impl<'hir> Ty<'hir> for ExprClosure<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct ExprBranch<'hir, 'input> {
    pub lhs: &'hir ExprPat<'hir, 'input>,
    pub rhs: &'hir ExprPat<'hir, 'input>,
    pub body: &'hir Expr<'hir, 'input>,
}

#[derive(Debug, Clone)]
pub enum ExprPat<'hir, 'input> {
    Bool(bool),
    I32(i32),
    Ident(ExprIdent<'hir, 'input>),
    Tuple(ExprTuple<'hir, ExprPat<'hir, 'input>>),
}

impl<'hir> Ty<'hir> for ExprPat<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        match self {
            ExprPat::Bool(_) => &Type::Bool,
            ExprPat::I32(_) => &Type::I32,
            ExprPat::Ident(ident) => ident.ty(),
            ExprPat::Tuple(tuple) => tuple.ty(),
        }
    }
}

// pub struct ExprTuple

#[derive(Debug, Clone)]
pub struct ExprAppl<'hir, 'input> {
    pub ty: &'hir Type<'hir>,
    pub lhs: &'hir Expr<'hir, 'input>,
    pub function: &'hir Expr<'hir, 'input>,
    pub rhs: &'hir Expr<'hir, 'input>,
}

impl<'hir> Ty<'hir> for ExprAppl<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}
