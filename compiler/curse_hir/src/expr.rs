use crate::{List, Type, TypeFunction};
use std::fmt;

pub trait Ty<'hir> {
    fn ty(&self) -> Type<'hir>;
}

/// A cheap `Copy` enum representing an expression.
#[derive(Copy, Clone, Debug)]
pub enum Expr<'hir, 'input> {
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
        branches: &'hir List<'hir, ExprBranch<'hir, 'input>>,
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
        match self {
            Expr::Builtin(builtin) => builtin.ty(),
            Expr::I32(_) => Type::I32,
            Expr::Bool(_) => Type::Bool,
            Expr::Unit => Type::Unit,
            Expr::Ident { ty, .. } => *ty,
            Expr::Tuple { ty, .. } => *ty,
            Expr::Closure { ty, .. } => *ty,
            Expr::Appl { ty, .. } => *ty,
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
        // These indices are enforced by by `Env::new`, which starts by placing
        // the types for these operators at these indices.
        match self {
            Add | Sub | Mul | Rem => Type::Function(&TypeFunction {
                lhs: Type::I32,
                rhs: Type::I32,
                output: Type::I32,
            }),
            Div => todo!("Type of div"),
            Eq | Lt | Gt | Le | Ge => Type::Function(&TypeFunction {
                lhs: Type::I32,
                rhs: Type::I32,
                output: Type::Bool,
            }),
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
pub struct ExprBranch<'hir, 'input> {
    pub lhs: Pat<'hir, 'input>,
    pub rhs: Pat<'hir, 'input>,
    pub body: Expr<'hir, 'input>,
}

#[derive(Copy, Clone, Debug)]
pub enum Pat<'hir, 'input> {
    Bool(bool),
    I32(i32),
    Unit,
    Ident {
        ty: Type<'hir>,
        literal: &'input str,
    },
    Tuple {
        ty: Type<'hir>,
        pats: &'hir List<'hir, Self>,
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
        match self {
            Pat::Bool(_) => Type::Bool,
            Pat::I32(_) => Type::I32,
            Pat::Unit => Type::Unit,
            Pat::Ident { ty, .. } => *ty,
            Pat::Tuple { ty, .. } => *ty,
        }
    }
}
