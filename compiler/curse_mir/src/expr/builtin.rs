
use crate::{Type, TypeFunction, TypeKind};
use std::fmt;

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

    pub fn type_kind(&self) -> TypeKind<'static> {
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

