use crate::{Type, TypeFunction, TypeKind, Var};
use curse_ast::{tok, Span};
use std::fmt;

pub trait Ty<'hir, 'input> {
    fn ty(&self) -> Type<'hir, 'input>;
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
        ty: TypeKind<'hir, 'input>,
        literal: &'input str,
    },
    Tuple {
        ty: TypeKind<'hir, 'input>,
        exprs: &'hir [Expr<'hir, 'input>],
    },
    Closure {
        ty: TypeKind<'hir, 'input>,
        arms: &'hir [ExprArm<'hir, 'input>],
    },
    Appl {
        ty: TypeKind<'hir, 'input>,
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

impl<'hir, 'input> Ty<'hir, 'input> for Expr<'hir, 'input> {
    fn ty(&self) -> Type<'hir, 'input> {
        match self.kind {
            ExprKind::Builtin(builtin) => Type {
                kind: builtin.ty_kind(),
                span: self.span,
            },
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

    fn ty_kind(&self) -> TypeKind<'static, 'static> {
        use Builtin::*;
        match self {
            Add | Sub | Mul | Rem => TypeKind::Function(&TypeFunction {
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
            Div => todo!("Type of div"),
            Eq | Lt | Gt | Le | Ge => TypeKind::Function(&TypeFunction {
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
            Print => todo!("Type of print"),
        }
    }
}

impl<'hir, 'input> Ty<'hir, 'input> for Builtin {
    fn ty(&self) -> Type<'hir, 'input> {
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
    pub open: tok::Pipe,
    pub lhs: Pat<'hir, 'input>,
    pub rhs: Pat<'hir, 'input>,
    pub close: tok::Pipe,
    pub body: Expr<'hir, 'input>,
}

impl<'hir, 'input> ExprArm<'hir, 'input> {
    pub fn dummy() -> Self {
        ExprArm {
            open: tok::Pipe::default(),
            lhs: Pat::dummy(),
            rhs: Pat::dummy(),
            close: tok::Pipe::default(),
            body: Expr::dummy(),
        }
    }
}

impl Span for ExprArm<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.open.span_between(self.close)
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
        ty: TypeKind<'hir, 'input>,
        literal: &'input str,
    },
    Tuple {
        ty: TypeKind<'hir, 'input>,
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

impl<'hir, 'input> Ty<'hir, 'input> for Pat<'hir, 'input> {
    fn ty(&self) -> Type<'hir, 'input> {
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
