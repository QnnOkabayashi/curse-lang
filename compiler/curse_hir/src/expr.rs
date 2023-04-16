use crate::{
    spanned::{WithSpan, S},
    List, Type, TypeFunction,
};
use std::fmt;

pub trait Ty<'hir> {
    fn ty(&self) -> S<Type<'hir>>;
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
        ty: S<Type<'hir>>,
        literal: &'input str,
    },
    Tuple {
        ty: S<Type<'hir>>,
        exprs: &'hir List<'hir, S<Expr<'hir, 'input>>>,
    },
    Closure {
        ty: S<Type<'hir>>,
        arms: &'hir List<'hir, ExprArm<'hir, 'input>>,
    },
    Appl {
        ty: S<Type<'hir>>,
        appl: &'hir ExprAppl<'hir, 'input>,
    },
}

#[derive(Debug)]
// #[displaydoc("{lhs} {function} {rhs}")]
pub struct ExprAppl<'hir, 'input> {
    pub lhs: S<Expr<'hir, 'input>>,
    pub function: S<Expr<'hir, 'input>>,
    pub rhs: S<Expr<'hir, 'input>>,
}

impl<'hir> Ty<'hir> for S<Expr<'hir, '_>> {
    fn ty(&self) -> S<Type<'hir>> {
        match *self.value() {
            Expr::Builtin(builtin) => builtin.ty(),
            Expr::I32(_) => Type::I32.with_span(self.span()),
            Expr::Bool(_) => Type::Bool.with_span(self.span()),
            Expr::Unit => Type::Unit.with_span(self.span()),
            Expr::Ident { ty, .. } => ty,
            Expr::Tuple { ty, .. } => ty,
            Expr::Closure { ty, .. } => ty,
            Expr::Appl { ty, .. } => ty,
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
    fn ty(&self) -> S<Type<'hir>> {
        use Builtin::*;
        match self {
            // TODO(quinn): how to we represent the source spans
            // of builtin values like (+)?
            Add | Sub | Mul | Rem => Type::Function(&TypeFunction {
                lhs: S(Type::I32, (0, 0)),
                rhs: S(Type::I32, (0, 0)),
                output: S(Type::I32, (0, 0)),
            })
            .with_span((0, 0)),
            Div => todo!("Type of div"),
            Eq | Lt | Gt | Le | Ge => Type::Function(&TypeFunction {
                lhs: S(Type::I32, (0, 0)),
                rhs: S(Type::I32, (0, 0)),
                output: S(Type::I32, (0, 0)),
            })
            .with_span((0, 0)),
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
    pub lhs: S<Pat<'hir, 'input>>,
    pub rhs: S<Pat<'hir, 'input>>,
    pub body: S<Expr<'hir, 'input>>,
}

#[derive(Copy, Clone, Debug)]
pub enum Pat<'hir, 'input> {
    Bool(bool),
    I32(i32),
    Unit,
    Ident {
        ty: S<Type<'hir>>,
        literal: &'input str,
    },
    Tuple {
        ty: S<Type<'hir>>,
        pats: &'hir List<'hir, S<Self>>,
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

impl<'hir> Ty<'hir> for S<Pat<'hir, '_>> {
    fn ty(&self) -> S<Type<'hir>> {
        match self.value() {
            Pat::Bool(_) => Type::Bool.with_span(self.span()),
            Pat::I32(_) => Type::I32.with_span(self.span()),
            Pat::Unit => Type::Unit.with_span(self.span()),
            Pat::Ident { ty, .. } => (*ty.value()).with_span(self.span()),
            Pat::Tuple { ty, .. } => (*ty.value()).with_span(self.span()),
        }
    }
}
