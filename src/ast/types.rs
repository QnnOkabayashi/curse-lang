#![allow(dead_code)]

use crate::ast;
use crate::lex::tok;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Type<'ast, 'input> {
    Named(Named<'input>),
    Tuple(Tuple<'ast, 'input>),
    Function(Function<'ast, 'input>),
}

impl fmt::Display for Type<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Named(name) => name.fmt(f),
            Type::Tuple(tuple) => tuple.fmt(f),
            Type::Function(function) => function.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Named<'input> {
    name: tok::Ident<'input>,
}

impl<'input> Named<'input> {
    pub fn new(name: tok::Ident<'input>) -> Self {
        Self { name }
    }
}

impl fmt::Display for Named<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name.literal)
    }
}

#[derive(Debug, Clone)]
pub struct Tuple<'ast, 'input> {
    tuple: ast::Tuple<&'ast Type<'ast, 'input>>,
}

impl<'ast, 'input> Tuple<'ast, 'input> {
    pub fn new(tuple: ast::Tuple<&'ast Type<'ast, 'input>>) -> Self {
        Self { tuple }
    }
}

impl fmt::Display for Tuple<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        if let Some((first, rest)) = self.tuple.elements.split_first() {
            write!(f, "{}", first.0)?;
            for val in rest {
                write!(f, ", {}", val.0)?;
            }
        }

        if let Some(trailing) = &self.tuple.trailing {
            write!(f, ", {trailing}")?;
        }

        write!(f, ")")
    }
}

// TODO: rework arena to allocate types as well to avoid Box
#[derive(Debug, Clone)]
pub struct Function<'ast, 'input> {
    lhs: &'ast Type<'ast, 'input>,
    rhs: &'ast Type<'ast, 'input>,
    arrow: tok::Arrow,
    ret: &'ast Type<'ast, 'input>,
}

impl<'ast, 'input> Function<'ast, 'input> {
    pub fn new(
        lhs: &'ast Type<'ast, 'input>,
        rhs: &'ast Type<'ast, 'input>,
        arrow: tok::Arrow,
        ret: &'ast Type<'ast, 'input>,
    ) -> Self {
        Self {
            lhs,
            rhs,
            arrow,
            ret,
        }
    }
}

impl fmt::Display for Function<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} -> {}", self.lhs, self.rhs, self.ret)
    }
}
