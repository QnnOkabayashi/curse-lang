use crate::{Cons, Type, TypeFunction};
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
    #[displaydoc("()")]
    Unit,
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
            Expr::Unit => &Type::Unit,
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
    pub ty: &'hir Type<'hir>,
    pub exprs: &'hir Cons<'hir, &'hir T>,
}

impl<'hir, T> Ty<'hir> for ExprTuple<'hir, T> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
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

#[derive(Debug, Clone)]
pub struct ExprClosure<'hir, 'input> {
    pub ty: &'hir Type<'hir>,
    pub branches: &'hir Cons<'hir, ExprBranch<'hir, 'input>>,
}

impl<'hir> Ty<'hir> for ExprClosure<'hir, '_> {
    fn ty(&'hir self) -> &'hir Type<'hir> {
        self.ty
    }
}

impl fmt::Display for ExprClosure<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.branches.item)?;
        for branch in self.branches.next.iter() {
            write!(f, " else {}", branch.item)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, Display)]
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
    #[displaydoc("()")]
    Unit,
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
            ExprPat::Unit => &Type::Unit,
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

pub use impl_dot::Graph;

mod impl_dot {
    use super::{Expr, Ty};

    pub struct Graph<'g> {
        expr: &'g Expr<'g, 'g>,
    }

    impl<'g> Graph<'g> {
        pub fn new(expr: &'g Expr<'g, 'g>) -> Self {
            Graph { expr }
        }
    }

    type Nd<'g> = &'g Expr<'g, 'g>;
    type Ed<'g> = (Nd<'g>, Nd<'g>);

    impl<'g> dot::GraphWalk<'g, Nd<'g>, Ed<'g>> for Graph<'g> {
        fn nodes(&'g self) -> dot::Nodes<'g, Nd<'g>> {
            fn expr_nodes<'g>(expr: Nd<'g>, nodes: &mut Vec<Nd<'g>>) {
                nodes.push(expr);
                match expr {
                    Expr::Builtin(_) => {}
                    Expr::Bool(_) => {}
                    Expr::I32(_) => {}
                    Expr::Unit => {}
                    Expr::Ident(_) => {}
                    Expr::Tuple(tuple) => {
                        for element in tuple.exprs.iter() {
                            expr_nodes(element, nodes);
                        }
                    }
                    Expr::Closure(closure) => {
                        for branch in closure.branches.iter() {
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
            expr_nodes(self.expr, &mut nodes);
            nodes.into()
        }

        fn edges(&'g self) -> dot::Edges<'g, Ed<'g>> {
            fn expr_edges<'g>(expr: Nd<'g>, parent: Option<Nd<'g>>, edges: &mut Vec<Ed<'g>>) {
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
                        for branch in closure.branches.iter() {
                            expr_edges(branch.body, Some(expr), edges);
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
            expr_edges(self.expr, None, &mut edges);
            edges.into()
        }

        fn source(&'g self, edge: &Ed<'g>) -> Nd<'g> {
            edge.0
        }

        fn target(&'g self, edge: &Ed<'g>) -> Nd<'g> {
            edge.1
        }
    }

    impl<'g> dot::Labeller<'g, Nd<'g>, Ed<'g>> for Graph<'g> {
        fn graph_id(&'g self) -> dot::Id<'g> {
            dot::Id::new("example").unwrap()
        }

        fn node_id(&'g self, n: &Nd<'g>) -> dot::Id<'g> {
            let x = *n as *const _ as usize;
            dot::Id::new(format!("p{}", x)).unwrap()
        }

        fn node_label(&'g self, n: &Nd<'g>) -> dot::LabelText<'g> {
            // TODO(quinn): can't just make an easy eval type function because
            // you can have typevars in tuples and functions and such and will
            // need to construct a new one of those if they contain a typevar.
            // So we'll end up creating a bunch of new allocations in the arena
            // to make these new types. And at that point we may as well lower
            // again to have concrete types and no dependence on a `&[Typevar]`
            // for lookups.

            match n {
                Expr::Builtin(builtin) => dot::LabelText::LabelStr(
                    format!("{}: {}", builtin.as_str(), builtin.ty()).into(),
                ),
                Expr::Bool(true) => dot::LabelText::LabelStr("true: bool".into()),
                Expr::Bool(false) => dot::LabelText::LabelStr("false: bool".into()),
                Expr::I32(int) => dot::LabelText::LabelStr(format!("{int}: i32").into()),
                Expr::Unit => dot::LabelText::LabelStr("(): ()".into()),
                Expr::Ident(ident) => {
                    dot::LabelText::LabelStr(format!("{}: {}", ident.literal, ident.ty()).into())
                }
                Expr::Tuple(tuple) => {
                    dot::LabelText::LabelStr(format!("{}: {}", tuple, tuple.ty()).into())
                }
                Expr::Closure(closure) => {
                    dot::LabelText::LabelStr(format!("<closure>: {}", closure.ty()).into())
                }
                Expr::Appl(appl) => {
                    dot::LabelText::LabelStr(format!("<appl>: {}", appl.ty()).into())
                }
            }
        }
    }
}
