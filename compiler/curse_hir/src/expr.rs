use crate::{BoxedTypeFunction, List, Type};
use displaydoc::Display;
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
        exprs: &'hir List<'hir, Self>,
    },
    Closure {
        ty: Type<'hir>,
        branches: &'hir List<'hir, ExprBranch<'hir, 'input>>,
    },
    Appl {
        ty: Type<'hir>,
        appl: &'hir BoxedExprAppl<'hir, 'input>,
    },
}

#[derive(Debug, Display)]
#[displaydoc("{lhs} {function} {rhs}")]
pub struct BoxedExprAppl<'hir, 'input> {
    pub lhs: Expr<'hir, 'input>,
    pub function: Expr<'hir, 'input>,
    pub rhs: Expr<'hir, 'input>,
}

// stack of ideas
// - Make expression trees graphable using arenas
//   - Be able to display singleton exprs like () and +, which we want to
//     statically allocate instead of put in an arena
//     - Inline singleton exprs into `Expr`, with pointers into arenas for
//       more complex items like tuples and applications
//       - Inline as many things as possible, like numbers and idents.
//         Harder to inline tuples and closures because they inflate the size
//         of `Expr` to 32 bytes.
//         - Come up with a way to not have to put the types within `Tuple` and
//           `Closure`,

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

impl fmt::Display for Expr<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Builtin(builtin) => builtin.fmt(f),
            Expr::I32(int) => int.fmt(f),
            Expr::Bool(b) => b.fmt(f),
            Expr::Unit => f.write_str("()"),
            Expr::Ident { literal, .. } => literal.fmt(f),
            Expr::Tuple { exprs, .. } => {
                write!(f, "(")?;
                write!(f, "{}", exprs.item)?;
                if let Some(remaining) = exprs.next {
                    for item in remaining.iter() {
                        write!(f, ", {item}")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Closure { branches, .. } => {
                write!(f, "{}", branches.item)?;
                if let Some(branches) = branches.next {
                    for branch in branches.iter() {
                        write!(f, " else {}", branch)?;
                    }
                }
                Ok(())
            }
            Expr::Appl { appl, .. } => appl.fmt(f),
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
    Print,
}

impl Builtin {
    pub fn as_str(&self) -> &'static str {
        match self {
            Builtin::Add => "+",
            Builtin::Sub => "-",
            Builtin::Mul => "*",
            Builtin::Rem => "%",
            Builtin::Div => "/",
            Builtin::Eq => "=",
            Builtin::Lt => "<",
            Builtin::Gt => ">",
            Builtin::Le => "<=",
            Builtin::Ge => ">=",
            Builtin::Print => "print",
        }
    }
}

impl<'hir> Ty<'hir> for Builtin {
    fn ty(&self) -> Type<'hir> {
        match self {
            Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Rem => {
                Type::Function(&BoxedTypeFunction {
                    lhs: Type::I32,
                    rhs: Type::I32,
                    output: Type::I32,
                })
            }
            Builtin::Div => todo!("Type of div"),
            Builtin::Eq | Builtin::Lt | Builtin::Gt | Builtin::Le | Builtin::Ge => {
                Type::Function(&BoxedTypeFunction {
                    lhs: Type::I32,
                    rhs: Type::I32,
                    output: Type::Bool,
                })
            }
            Builtin::Print => todo!("Type of print"),
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("{literal}")]
pub struct ExprIdent<'hir, 'input> {
    pub literal: &'input str,
    pub ty: Type<'hir>,
}

impl<'hir> Ty<'hir> for ExprIdent<'hir, '_> {
    fn ty(&self) -> Type<'hir> {
        self.ty
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ExprTuple<'hir, T> {
    pub ty: Type<'hir>,
    pub exprs: &'hir List<'hir, T>,
}

impl<'hir, T> Ty<'hir> for ExprTuple<'hir, T> {
    fn ty(&self) -> Type<'hir> {
        self.ty
    }
}

impl<T: fmt::Display> fmt::Display for ExprTuple<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        write!(f, "{}", self.exprs.item)?;
        if let Some(remaining) = self.exprs.next {
            for item in remaining.iter() {
                write!(f, ", {item}")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("|{lhs}, {rhs}| {body}")]
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
        exprs: &'hir List<'hir, Pat<'hir, 'input>>,
    },
}

impl fmt::Display for Pat<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Bool(b) => b.fmt(f),
            Pat::I32(i) => i.fmt(f),
            Pat::Unit => f.write_str("()"),
            Pat::Ident { literal, .. } => f.write_str(literal),
            Pat::Tuple { exprs, .. } => {
                write!(f, "(")?;
                write!(f, "{}", exprs.item)?;
                if let Some(remaining) = exprs.next {
                    for item in remaining.iter() {
                        write!(f, ", {item}")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

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
