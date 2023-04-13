use crate::{arena::P, List, Type};
use std::fmt;

pub trait Ty {
    fn ty(&self) -> Type;
}

/// A cheap `Copy` enum representing an expression.
#[derive(Copy, Clone, Debug)]
pub enum Expr<'input> {
    Builtin(Builtin),
    I32(i32),
    Bool(bool),
    Unit,
    // TODO(quinn): Micro optimization opportunity here! If we do string interning
    // and use u32 as the ids, we can make `Expr` be 16 bytes instead of 32.
    // But would also have to simplify tuple/closure/appl to not have types inlined
    Ident {
        ty: Type,
        literal: &'input str,
    },
    Tuple {
        ty: Type,
        exprs: P<List<Expr<'input>>>,
    },
    Closure {
        ty: Type,
        branches: P<List<ExprBranch<'input>>>,
    },
    Appl {
        ty: Type,
        appl: P<BoxedExprAppl<'input>>,
    },
}

#[derive(Debug)]
// #[displaydoc("{lhs} {function} {rhs}")]
pub struct BoxedExprAppl<'input> {
    pub lhs: Expr<'input>,
    pub function: Expr<'input>,
    pub rhs: Expr<'input>,
}

impl Ty for Expr<'_> {
    fn ty(&self) -> Type {
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

impl Ty for Builtin {
    fn ty(&self) -> Type {
        use Builtin::*;
        // These indices are enforced by by `Env::new`, which starts by placing
        // the types for these operators at these indices.
        match self {
            Add | Sub | Mul | Rem => Type::Function(P::new(0)),
            Div => todo!("Type of div"),
            Eq | Lt | Gt | Le | Ge => Type::Function(P::new(1)),
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
pub struct ExprBranch<'input> {
    pub lhs: Pat<'input>,
    pub rhs: Pat<'input>,
    pub body: Expr<'input>,
}

#[derive(Copy, Clone, Debug)]
pub enum Pat<'input> {
    Bool(bool),
    I32(i32),
    Unit,
    Ident {
        ty: Type,
        literal: &'input str,
    },
    Tuple {
        ty: Type,
        exprs: P<List<Pat<'input>>>,
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

impl Ty for Pat<'_> {
    fn ty(&self) -> Type {
        match self {
            Pat::Bool(_) => Type::Bool,
            Pat::I32(_) => Type::I32,
            Pat::Unit => Type::Unit,
            Pat::Ident { ty, .. } => *ty,
            Pat::Tuple { ty, .. } => *ty,
        }
    }
}
