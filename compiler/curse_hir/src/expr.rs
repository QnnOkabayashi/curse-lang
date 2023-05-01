use crate::{Type, TypeFunction, TypeKind, Var};
use std::fmt;

pub trait Ty<'hir> {
    fn ty(&self) -> Type<'hir>;
}

#[derive(Copy, Clone, Debug)]
pub struct Expr<'hir, 'input> {
    pub kind: ExprKind<'hir, 'input>,
    pub span: (usize, usize),
}

impl<'hir, 'input> Expr<'hir, 'input> {
    pub fn dummy() -> Self {
        Expr {
            kind: ExprKind::unit(),
            span: (0, 0),
        }
    }
}

/// A cheap `Copy` enum representing an expression.
#[derive(Copy, Clone, Debug)]
pub enum ExprKind<'hir, 'input> {
    Builtin(Builtin),
    I32(i32),
    Bool(bool),
    Ident {
        ty: TypeKind<'hir>,
        literal: &'input str,
    },
    Tuple {
        ty: TypeKind<'hir>,
        exprs: &'hir [Expr<'hir, 'input>],
    },
    Closure {
        ty: TypeKind<'hir>,
        arm1: &'hir ExprArm<'hir, 'input>,
        arms: &'hir [ExprArm<'hir, 'input>],
    },
    Appl {
        ty: TypeKind<'hir>,
        appl: &'hir ExprAppl<'hir, 'input>,
    },
}

impl<'hir, 'input> ExprKind<'hir, 'input> {
    fn unit() -> Self {
        ExprKind::Tuple {
            ty: TypeKind::Tuple(&[]),
            exprs: &[],
        }
    }
}

#[derive(Debug)]
// #[displaydoc("{lhs} {function} {rhs}")]
pub struct ExprAppl<'hir, 'input> {
    pub lhs: Expr<'hir, 'input>,
    pub function: Expr<'hir, 'input>,
    pub rhs: Expr<'hir, 'input>,
}

impl<'hir> Ty<'hir> for Expr<'hir, '_> {
    fn ty(&self) -> Type<'hir> {
        match self.kind {
            ExprKind::Builtin(builtin) => builtin.ty(),
            ExprKind::I32(_) => Type {
                kind: TypeKind::I32,
                span: self.span,
            },
            ExprKind::Bool(_) => Type {
                kind: TypeKind::Bool,
                span: self.span,
            },
            ExprKind::Ident { ty, .. } => Type {
                kind: ty,
                span: self.span,
            },
            ExprKind::Tuple { ty, .. } => Type {
                kind: ty,
                span: self.span,
            },
            ExprKind::Closure { ty, .. } => Type {
                kind: ty,
                span: self.span,
            },
            ExprKind::Appl { ty, .. } => Type {
                kind: ty,
                span: self.span,
            },
        }
    }
}

// impl fmt::Display for Expr<'_, '_> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Expr::Builtin(builtin) => builtin.fmt(f),
//             Expr::I32(int) => int.fmt(f),
//             Expr::Bool(b) => b.fmt(f),
//             Expr::Unit => f.write_str("()"),
//             Expr::Ident { literal, .. } => literal.fmt(f),
//             Expr::Tuple { exprs, .. } => {
//                 write!(f, "({})", exprs.delim(", "))
//             }
//             Expr::Closure { branches, .. } => {
//                 write!(f, "{}", branches.delim(" else "))
//             }
//             Expr::Appl { appl, .. } => appl.fmt(f),
//         }
//     }
// }

#[derive(Copy, Clone, Debug)]
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

impl Builtin {
    pub fn as_str(&self) -> &'static str {
        use Builtin::*;
        match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Rem => "%",
            Div => "/",
            Eq => "=",
            Lt => "<",
            Gt => ">",
            Le => "<=",
            Ge => ">=",
            Print => "print",
        }
    }
}

impl<'hir> Ty<'hir> for Builtin {
    fn ty(&self) -> Type<'hir> {
        use Builtin::*;
        match self {
            // TODO(quinn): how to we represent the source spans
            // of builtin values like (+)?
            Add | Sub | Mul | Rem => Type {
                kind: TypeKind::Function(&TypeFunction {
                    lhs: Type {
                        kind: TypeKind::I32,
                        span: (0, 0),
                    },
                    rhs: Type {
                        kind: TypeKind::I32,
                        span: (0, 0),
                    },
                    output: Type {
                        kind: TypeKind::I32,
                        span: (0, 0),
                    },
                }),
                span: (0, 0),
            },
            Div => todo!("Type of div"),
            Eq | Lt | Gt | Le | Ge => Type {
                kind: TypeKind::Function(&TypeFunction {
                    lhs: Type {
                        kind: TypeKind::I32,
                        span: (0, 0),
                    },
                    rhs: Type {
                        kind: TypeKind::I32,
                        span: (0, 0),
                    },
                    output: Type {
                        kind: TypeKind::I32,
                        span: (0, 0),
                    },
                }),
                span: (0, 0),
            },
            Print => todo!("Type of print"),
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Copy, Clone, Debug)]
// #[displaydoc("|{lhs}, {rhs}| {body}")]
pub struct ExprArm<'hir, 'input> {
    pub lhs: Pat<'hir, 'input>,
    pub rhs: Pat<'hir, 'input>,
    pub body: Expr<'hir, 'input>,
}

impl<'hir, 'input> ExprArm<'hir, 'input> {
    pub fn dummy() -> Self {
        ExprArm {
            lhs: Pat::dummy(),
            rhs: Pat::dummy(),
            body: Expr::dummy(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Pat<'hir, 'input> {
    pub kind: PatKind<'hir, 'input>,
    pub span: (usize, usize),
}

impl<'hir, 'input> Pat<'hir, 'input> {
    pub fn dummy() -> Self {
        Pat {
            kind: PatKind::unit(),
            span: (0, 0),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PatKind<'hir, 'input> {
    Bool(bool),
    I32(i32),
    Ident {
        ty: TypeKind<'hir>,
        literal: &'input str,
    },
    Tuple {
        ty: TypeKind<'hir>,
        pats: &'hir [Pat<'hir, 'input>],
    },
    /// An omitted pattern.
    ///
    /// For example, the rhs pattern in `|x| ...` would be `Omitted`
    /// because it is omitted.
    Omitted(Var),
}

impl<'hir, 'input> PatKind<'hir, 'input> {
    pub fn unit() -> Self {
        PatKind::Tuple {
            ty: TypeKind::unit(),
            pats: &[],
        }
    }
}

impl<'hir> Ty<'hir> for Pat<'hir, '_> {
    fn ty(&self) -> Type<'hir> {
        match self.kind {
            PatKind::Bool(_) => Type {
                kind: TypeKind::Bool,
                span: self.span,
            },
            PatKind::I32(_) => Type {
                kind: TypeKind::I32,
                span: self.span,
            },
            PatKind::Ident { ty, .. } => Type {
                kind: ty,
                span: self.span,
            },
            PatKind::Tuple { ty, .. } => Type {
                kind: ty,
                span: self.span,
            },
            PatKind::Omitted(var) => Type {
                kind: TypeKind::Var(var),
                span: self.span,
            },
        }
    }
}
