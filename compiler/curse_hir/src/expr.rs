use crate::{List, Type, TypeFunction, TypeKind};
use std::fmt;

pub trait Ty<'hir> {
    fn ty(&self) -> Type<'hir>;
}

#[derive(Copy, Clone, Debug)]
pub struct Expr<'hir, 'input> {
    pub kind: ExprKind<'hir, 'input>,
    pub span: (usize, usize),
}

/// A cheap `Copy` enum representing an expression.
#[derive(Copy, Clone, Debug)]
pub enum ExprKind<'hir, 'input> {
    Builtin(Builtin),
    I32(i32),
    Bool(bool),
    Unit,
    // TODO(quinn): Micro optimization opportunity here! If we do string interning
    // and use u32 as the ids, we can make `Expr` be 16 bytes instead of 32.
    // But would also have to simplify tuple/closure/appl to not have types inlined
    Ident {
        ty: Type<'hir>,
        literal: &'input str,
    },
    Tuple {
        ty: Type<'hir>,
        exprs: &'hir List<'hir, Expr<'hir, 'input>>,
    },
    Closure {
        ty: Type<'hir>,
        arms: &'hir List<'hir, ExprArm<'hir, 'input>>,
    },
    Appl {
        ty: Type<'hir>,
        appl: &'hir ExprAppl<'hir, 'input>,
    },
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
            ExprKind::Unit => Type {
                kind: TypeKind::Unit,
                span: self.span,
            },
            ExprKind::Ident { ty, .. } => ty,
            ExprKind::Tuple { ty, .. } => ty,
            ExprKind::Closure { ty, .. } => ty,
            ExprKind::Appl { ty, .. } => ty,
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

#[derive(Copy, Clone, Debug)]
pub struct Pat<'hir, 'input> {
    pub kind: PatKind<'hir, 'input>,
    pub span: (usize, usize),
}

#[derive(Copy, Clone, Debug)]
pub enum PatKind<'hir, 'input> {
    Bool(bool),
    I32(i32),
    Unit,
    Ident {
        ty: Type<'hir>,
        literal: &'input str,
    },
    Tuple {
        ty: Type<'hir>,
        pats: &'hir List<'hir, Pat<'hir, 'input>>,
    },
}

// impl fmt::Display for Pat<'_, '_> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Pat::Bool(b) => b.fmt(f),
//             Pat::I32(i) => i.fmt(f),
//             Pat::Unit => f.write_str("()"),
//             Pat::Ident { literal, .. } => f.write_str(literal),
//             Pat::Tuple { exprs, .. } => write!(f, "({})", exprs.delim(", ")),
//         }
//     }
// }

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
            PatKind::Unit => Type {
                kind: TypeKind::Unit,
                span: self.span,
            },
            PatKind::Ident { ty, .. } => Type {
                kind: ty.kind,
                span: self.span,
            },
            PatKind::Tuple { ty, .. } => Type {
                kind: ty.kind,
                span: self.span,
            },
        }
    }
}
