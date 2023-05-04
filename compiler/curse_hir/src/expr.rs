use crate::{Type, TypeChoice, TypeFunction, TypeKind};
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
                kind: builtin.type_kind(),
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
        }
    }

    pub fn type_kind(&self) -> TypeKind<'static, 'static> {
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
        ty: &'hir [Type<'hir, 'input>],
        pats: &'hir [Pat<'hir, 'input>],
    },
    Choice {
        ty: &'hir TypeChoice<'hir, 'input>,
        variant: u32,
        payload: Option<&'hir Pat<'hir, 'input>>,
    },
    /// An omitted pattern.
    ///
    /// For example, the rhs pattern in `|x| ...` would be `Omitted`
    /// because there's only a lhs pattern.
    Omitted,
}

impl<'hir, 'input> PatKind<'hir, 'input> {
    pub fn unit() -> Self {
        PatKind::Tuple { ty: &[], pats: &[] }
    }
}

impl<'hir, 'input> Ty<'hir, 'input> for Pat<'hir, 'input> {
    fn ty(&self) -> Type<'hir, 'input> {
        let kind = match self.kind {
            PatKind::Bool(_) => TypeKind::Bool,
            PatKind::I32(_) => TypeKind::I32,
            PatKind::Ident { ty, .. } => ty,
            PatKind::Tuple { ty, .. } => TypeKind::Tuple(ty),
            PatKind::Choice { ty, .. } => TypeKind::Choice(ty),
            PatKind::Omitted => TypeKind::Tuple(&[]),
        };

        Type {
            kind,
            span: self.span,
        }
    }
}
