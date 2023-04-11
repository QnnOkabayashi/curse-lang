//! Utilities for writing an AST in dot format for Graphviz.
use crate::{expr::Ty, Expr};
use std::fmt::Write as _;

pub struct Builder {
    count: u32,
    out: String,
}

impl Builder {
    pub fn new() -> Self {
        let out = String::with_capacity(2048);
        Builder {
            count: 0,
            out: out + "digraph example {",
        }
    }

    pub fn visit_expr(&mut self, expr: Expr<'_, '_>, parent: Option<u32>) {
        let id = self.fresh();
        self.out.push_str("\n    ");

        match expr {
            Expr::Builtin(builtin) => {
                write!(
                    self.out,
                    "p{id}[label = \"{builtin}: {ty}\"]",
                    ty = builtin.ty(),
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
                write!(self.out, "p{id}[label = \"{literal}: {ty}\"]").unwrap();
            }
            Expr::Tuple { ty, exprs } => {
                write!(self.out, "p{id}[label = \"tuple: {ty}\"]").unwrap();
                for element in exprs.iter() {
                    self.visit_expr(*element, Some(id));
                }
            }
            Expr::Closure { ty, branches } => {
                write!(self.out, "p{id}[label = \"<closure>: {ty}\"]").unwrap();
                for branch in branches.iter() {
                    self.visit_expr(branch.body, Some(id));
                }
            }
            Expr::Appl { ty, appl } => {
                write!(self.out, "p{id}[label = \"<appl>: {ty}\"]").unwrap();
                self.visit_expr(appl.lhs, Some(id));
                self.visit_expr(appl.function, Some(id));
                self.visit_expr(appl.rhs, Some(id));
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
