//! Utilities for writing an AST in dot format for Graphviz.
use crate::{expr::Ty, Env, Expr};
use std::fmt::Write as _;

pub struct Builder<'env> {
    env: &'env Env<'env, 'env>,
    count: u32,
    out: String,
}

impl<'env> Builder<'env> {
    pub fn new(env: &'env Env<'env, 'env>) -> Self {
        let out = String::with_capacity(2048) + "digraph example {";

        Builder { env, count: 0, out }
    }

    pub fn visit_expr(&mut self, expr: Expr<'env, '_>, parent: Option<u32>, name: Option<&str>) {
        let id = self.fresh();
        self.out += "\n    ";

        match expr {
            Expr::Builtin(builtin) => {
                write!(
                    self.out,
                    "p{id}[label = \"{builtin}: {ty}\"]",
                    ty = builtin.ty().pretty(self.env),
                )
                .unwrap();
            }
            Expr::I32(i) => {
                write!(self.out, "p{id}[label = \"{i}: i32\"]").unwrap();
            }
            Expr::Bool(b) => {
                write!(self.out, "p{id}[label = \"{b}: bool\"]").unwrap();
            }
            Expr::Unit => {
                write!(self.out, "p{id}[label = \"(): ()\"]").unwrap();
            }
            Expr::Ident { ty, literal } => {
                write!(
                    self.out,
                    "p{id}[label = \"{literal}: {ty}\"]",
                    ty = ty.pretty(self.env)
                )
                .unwrap();
            }
            Expr::Tuple { ty, exprs } => {
                write!(
                    self.out,
                    "p{id}[label = \"tuple: {ty}\"]",
                    ty = ty.pretty(self.env)
                )
                .unwrap();
                for expr in exprs.iter() {
                    self.visit_expr(*expr, Some(id), None);
                }
            }
            Expr::Closure { ty, branches } => {
                let name = name.unwrap_or("<closure>");
                write!(
                    self.out,
                    "p{id}[label = \"{name}: {ty}\"]",
                    ty = ty.pretty(self.env)
                )
                .unwrap();
                for branch in branches.iter() {
                    self.visit_expr(branch.body, Some(id), None);
                }
            }
            Expr::Appl { ty, appl } => {
                write!(
                    self.out,
                    "p{id}[label = \"<appl>: {ty}\"]",
                    ty = ty.pretty(self.env)
                )
                .unwrap();
                self.visit_expr(appl.lhs, Some(id), None);
                self.visit_expr(appl.function, Some(id), None);
                self.visit_expr(appl.rhs, Some(id), None);
            }
        }

        if let Some(parent_id) = parent {
            write!(self.out, "\n    p{parent_id} -> p{id}[label = \"\"]").unwrap();
        }
    }

    fn fresh(&mut self) -> u32 {
        self.count += 1;
        self.count
    }

    pub fn finish(self) -> String {
        self.out + "\n}"
    }
}
