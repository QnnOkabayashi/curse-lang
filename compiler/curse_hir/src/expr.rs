use crate::{Type, TypeFunction};
use displaydoc::Display;
use std::fmt;

pub trait Ty<'hir> {
    fn ty(&'hir self) -> &'hir Type<'hir>;
}

#[derive(Display, Debug, Clone)]
pub enum Expr<'hir, 'input> {
    #[displaydoc("{0}")]
    Builtin(Builtin),
    #[displaydoc("{0}")]
    Bool(bool),
    #[displaydoc("{0}")]
    I32(i32),
    #[displaydoc("{0}")]
    Ident(ExprIdent<'hir, 'input>),
    #[displaydoc("{0}")]
    Tuple(ExprTuple<'hir, Expr<'hir, 'input>>),
    #[displaydoc("{0}")]
    Closure(ExprClosure<'hir, 'input>),
    #[displaydoc("{0}")]
    Appl(ExprAppl<'hir, 'input>),
}

impl<'hir> Ty<'hir> for Expr<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        match self {
            Expr::Builtin(builtin) => builtin.ty(),
            Expr::Bool(_) => &Type::Bool,
            Expr::I32(_) => &Type::I32,
            Expr::Tuple(tuple) => tuple.ty(),
            Expr::Ident(ident) => ident.ty(),
            Expr::Closure(closure) => closure.ty(),
            Expr::Appl(appl) => appl.ty(),
        }
    }
}

#[derive(Debug, Clone)]
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
    fn ty(&'hir self) -> &'hir Type<'hir> {
        match self {
            Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Rem => {
                &Type::Function(TypeFunction {
                    lhs: &Type::I32,
                    rhs: &Type::I32,
                    output: &Type::I32,
                })
            }
            Builtin::Div => todo!("Type of div"),
            Builtin::Eq | Builtin::Lt | Builtin::Gt | Builtin::Le | Builtin::Ge => {
                &Type::Function(TypeFunction {
                    lhs: &Type::I32,
                    rhs: &Type::I32,
                    output: &Type::Bool,
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

#[derive(Display, Debug, Clone)]
#[displaydoc("{literal}")]
pub struct ExprIdent<'hir, 'input> {
    pub literal: &'input str,
    pub ty: &'hir Type<'hir>,
}

impl<'hir> Ty<'hir> for ExprIdent<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct ExprTuple<'hir, T> {
    pub exprs: Vec<&'hir T>,
    pub ty: &'hir Type<'hir>,
}

impl<'hir, T> Ty<'hir> for ExprTuple<'hir, T> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

impl<T: fmt::Display> fmt::Display for ExprTuple<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        if let Some((x, xs)) = self.exprs.split_first() {
            write!(f, "{x}")?;
            for x in xs {
                write!(f, ", {x}")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub struct ExprClosure<'hir, 'input> {
    pub ty: &'hir Type<'hir>,
    pub head: ExprBranch<'hir, 'input>,
    pub tail: Vec<ExprBranch<'hir, 'input>>,
}

impl<'hir, 'input> ExprClosure<'hir, 'input> {
    pub fn iter_branches(&self) -> impl Iterator<Item = &ExprBranch<'hir, 'input>> {
        Some(&self.head).into_iter().chain(self.tail.iter())
    }
}

impl<'hir> Ty<'hir> for ExprClosure<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

impl fmt::Display for ExprClosure<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.tail.is_empty() {
            self.head.fmt(f)
        } else {
            write!(f, "(")?;
            self.head.fmt(f)?;
            for branch in self.tail.iter() {
                write!(f, " else ")?;
                branch.fmt(f)?;
            }
            write!(f, ")")
        }
    }
}

#[derive(Display, Debug, Clone)]
#[displaydoc("|{lhs}, {rhs}| {body}")]
pub struct ExprBranch<'hir, 'input> {
    pub lhs: &'hir ExprPat<'hir, 'input>,
    pub rhs: &'hir ExprPat<'hir, 'input>,
    pub body: &'hir Expr<'hir, 'input>,
}

#[derive(Display, Debug, Clone)]
pub enum ExprPat<'hir, 'input> {
    #[displaydoc("{0}")]
    Bool(bool),
    #[displaydoc("{0}")]
    I32(i32),
    #[displaydoc("{0}")]
    Ident(ExprIdent<'hir, 'input>),
    #[displaydoc("{0}")]
    Tuple(ExprTuple<'hir, ExprPat<'hir, 'input>>),
}

impl<'hir> Ty<'hir> for ExprPat<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        match self {
            ExprPat::Bool(_) => &Type::Bool,
            ExprPat::I32(_) => &Type::I32,
            ExprPat::Ident(ident) => ident.ty(),
            ExprPat::Tuple(tuple) => tuple.ty(),
        }
    }
}

#[derive(Display, Debug, Clone)]
#[displaydoc("({lhs} {function} {rhs})")]
pub struct ExprAppl<'hir, 'input> {
    pub ty: &'hir Type<'hir>,
    pub lhs: &'hir Expr<'hir, 'input>,
    pub function: &'hir Expr<'hir, 'input>,
    pub rhs: &'hir Expr<'hir, 'input>,
}

impl<'hir> Ty<'hir> for ExprAppl<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

mod impl_dot {
    use super::{Expr, Ty};

    type Nd<'hir, 'input> = &'hir Expr<'hir, 'input>;
    type Ed<'hir, 'input> = (Nd<'hir, 'input>, Nd<'hir, 'input>);

    impl<'hir, 'input> dot::GraphWalk<'hir, Nd<'hir, 'input>, Ed<'hir, 'input>> for Expr<'hir, 'input> {
        fn nodes(&'hir self) -> dot::Nodes<'hir, Nd<'hir, 'input>> {
            fn expr_nodes<'hir, 'input>(
                expr: &'hir Expr<'hir, 'input>,
                nodes: &mut Vec<Nd<'hir, 'input>>,
            ) {
                nodes.push(expr);
                match expr {
                    Expr::Builtin(_) => {}
                    Expr::Bool(_) => {}
                    Expr::I32(_) => {}
                    Expr::Ident(_) => {}
                    Expr::Tuple(tuple) => {
                        for element in tuple.exprs.iter() {
                            expr_nodes(element, nodes);
                        }
                    }
                    Expr::Closure(closure) => {
                        for branch in closure.iter_branches() {
                            expr_nodes(branch.body, nodes);
                        }
                    }
                    Expr::Appl(appl) => {
                        expr_nodes(&appl.lhs, nodes);
                        expr_nodes(&appl.function, nodes);
                        expr_nodes(&appl.rhs, nodes);
                    }
                }
            }

            let mut nodes = Vec::with_capacity(64);
            expr_nodes(self, &mut nodes);
            nodes.into()
        }

        fn edges(&'hir self) -> dot::Edges<'hir, Ed<'hir, 'input>> {
            fn expr_edges<'hir, 'input>(
                expr: &'hir Expr<'hir, 'input>,
                parent: Option<&'hir Expr<'hir, 'input>>,
                edges: &mut Vec<Ed<'hir, 'input>>,
            ) {
                if let Some(parent) = parent {
                    edges.push((parent, expr));
                }
                match expr {
                    Expr::Tuple(tuple) => {
                        for element in tuple.exprs.iter() {
                            expr_edges(element, Some(expr), edges);
                        }
                    }
                    Expr::Closure(closure) => {
                        for branch in closure.iter_branches() {
                            expr_edges(branch.body, None, edges);
                        }
                    }
                    Expr::Appl(appl) => {
                        expr_edges(&appl.lhs, Some(expr), edges);
                        expr_edges(&appl.function, Some(expr), edges);
                        expr_edges(&appl.rhs, Some(expr), edges);
                    }
                    _ => {}
                }
            }
            
            let mut edges = Vec::with_capacity(64);
            expr_edges(self, None, &mut edges);
            edges.into()
        }

        fn source(&'hir self, edge: &Ed<'hir, 'input>) -> Nd<'hir, 'input> {
            edge.0
        }

        fn target(&'hir self, edge: &Ed<'hir, 'input>) -> Nd<'hir, 'input> {
            edge.1
        }
    }

    impl<'hir, 'input> dot::Labeller<'hir, Nd<'hir, 'input>, Ed<'hir, 'input>> for Expr<'hir, 'input> {
        fn graph_id(&'hir self) -> dot::Id<'hir> {
            dot::Id::new("example").unwrap()
        }

        fn node_id(&'hir self, n: &Nd<'hir, 'input>) -> dot::Id<'hir> {
            let x = *n as *const _ as usize;
            dot::Id::new(format!("p{}", x)).unwrap()
        }
    
        fn node_label(&'hir self, n: &Nd<'hir, 'input>) -> dot::LabelText<'hir> {
            match n {
                Expr::Builtin(builtin) => dot::LabelText::LabelStr(format!("{}: {}", builtin.as_str(), builtin.ty()).into()),
                Expr::Bool(true) => dot::LabelText::LabelStr("true: bool".into()),
                Expr::Bool(false) => dot::LabelText::LabelStr("false: bool".into()),
                Expr::I32(int) => dot::LabelText::LabelStr(format!("{int}: i32").into()),
                Expr::Ident(ident) => dot::LabelText::LabelStr(format!("{}: {}", ident.literal, ident.ty()).into()),
                Expr::Tuple(tuple) => dot::LabelText::LabelStr(format!("{}: {}", tuple, tuple.ty()).into()),
                Expr::Closure(closure) => dot::LabelText::LabelStr(format!("<closure>: {}", closure.ty()).into()),
                Expr::Appl(appl) => dot::LabelText::LabelStr(format!("<appl>: {}", appl.ty()).into()),
            }
        }
    }
}
