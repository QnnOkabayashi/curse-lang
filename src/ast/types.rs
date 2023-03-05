#![allow(dead_code)]

use crate::ast;
use crate::lex::tok;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Type {
    Integer(Integer),
    Tuple(Tuple),
    Function(Function),
    Unit(Unit),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer(integer) => integer.fmt(f),
            Type::Tuple(tuple) => tuple.fmt(f),
            Type::Function(function) => function.fmt(f),
            Type::Unit(unit) => unit.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    name: tok::I32,
}

impl Integer {
    pub fn new(name: tok::I32) -> Self {
        Self { name }
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "i32")
    }
}

#[derive(Debug, Clone)]
pub struct Tuple {
    tuple: ast::Tuple<Type>,
}

impl Tuple {
    pub fn new(tuple: ast::Tuple<Type>) -> Self {
        Self { tuple }
    }
}

impl fmt::Display for Tuple {
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
pub struct Function {
    lhs: Box<Type>,
    rhs: Box<Type>,
    arrow: tok::Arrow,
    ret: Box<Type>,
}

impl Function {
    pub fn new(lhs: Type, rhs: Type, arrow: tok::Arrow, ret: Type) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            arrow,
            ret: Box::new(ret),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} -> {}", self.lhs, self.rhs, self.ret)
    }
}

#[derive(Debug, Clone)]
pub struct Unit {
    lparen: tok::LParen,
    rparen: tok::RParen,
}

impl Unit {
    pub fn new(lparen: tok::LParen, rparen: tok::RParen) -> Self {
        Self { lparen, rparen }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "()")
    }
}
