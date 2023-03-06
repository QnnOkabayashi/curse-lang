#![allow(dead_code)]

use crate::ast;
use crate::lex::tok;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Type<'input> {
    Named(Named<'input>),
    Tuple(Tuple<'input>),
    Function(Function<'input>),
}

impl<'input> fmt::Display for Type<'input> {
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
pub struct Tuple<'input> {
    tuple: ast::Tuple<Type<'input>>,
}

impl<'input> Tuple<'input> {
    pub fn new(tuple: ast::Tuple<Type<'input>>) -> Self {
        Self { tuple }
    }
}

impl fmt::Display for Tuple<'_> {
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
pub struct Function<'input> {
    lhs: Box<Type<'input>>,
    rhs: Box<Type<'input>>,
    arrow: tok::Arrow,
    ret: Box<Type<'input>>,
}

impl<'input> Function<'input> {
    pub fn new(lhs: Type<'input>, rhs: Type<'input>, arrow: tok::Arrow, ret: Type<'input>) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            arrow,
            ret: Box::new(ret),
        }
    }
}

impl fmt::Display for Function<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} -> {}", self.lhs, self.rhs, self.ret)
    }
}
