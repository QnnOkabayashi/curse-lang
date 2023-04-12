// #![allow(dead_code)]
use curse_ast as ast;
use displaydoc::Display;
use petgraph::graph::NodeIndex;
use std::{collections::HashMap, fmt, num};
use thiserror::Error;

mod equations;
use equations::{Edge, Equations, Node};
mod expr;
use expr::*;
mod dot;

#[cfg(test)]
mod tests;

/// `Some` is bound, `None` is unbound.
pub type Typevar = Option<(Type, NodeIndex)>;

pub struct Display<F: Fn(&mut fmt::Formatter) -> fmt::Result>(F);

impl<F: Fn(&mut fmt::Formatter) -> fmt::Result> fmt::Display for Display<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

/// Newtype around a `usize` used to index into the `typevars` field of `Env`.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("T{0}")]
pub struct Var(u32);

// Just 8 bytes :D
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type {
    I32,
    Bool,
    Unit,
    Tuple(TupleItemExprIx),
    Var(Var),
    Function(BoxedTypeFunctionIx),
}

impl Type {
    /// Display a [`Type`] using a provided context.
    pub fn display<'a>(&'a self, env: &'a Env) -> impl fmt::Display + 'a {
        Display(move |f| match self {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Tuple(tuple) => {
                let base = &env.tuple_item_types_vec[..];
                let mut x = &base[ix(*tuple)];
                write!(f, "(")?;
                write!(f, "{}", x.item.display(env))?;
                while next(&mut x, base) {
                    write!(f, ", {}", x.item.display(env))?;
                }
                write!(f, ")")
            }
            Type::Var(var) => {
                if let Some((ty, _)) = &env.typevars[ix(var.0)] {
                    write!(f, "{}", ty.display(env))
                } else {
                    write!(f, "{}", var)
                }
            }
            Type::Function(fun) => {
                let fun = env.type_functions_vec[ix(*fun)];

                let lhs = fun.lhs.display(env);
                let rhs = fun.rhs.display(env);
                let output = fun.output.display(env);

                write!(f, "({lhs} {rhs} -> {output})")
            }
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
// #[displaydoc("({lhs} {rhs} -> {output})")]
pub struct BoxedTypeFunction {
    lhs: Type,
    rhs: Type,
    output: Type,
}

pub type ListIx = u32;

#[derive(Copy, Clone)]
pub struct List2<T> {
    item: T,
    next: Option<ListIx>,
}

impl<T> List2<T> {
    /// Returns the number of elements in the list.
    // Never empty
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self, base: &[Self]) -> usize {
        // TODO(quinn): make this iterative
        1 + self.next.map(|i| base[ix(i)].len(base)).unwrap_or(0)
    }

    // /// Returns the next element in the linked list.
    // pub fn next<'a>(&self, base: &'a [Self]) -> Option<&List2<T>> {
    //     self.next.map(|index| &base[ix(index)])
    // }
}

pub fn next<'a, T>(list: &mut &'a List2<T>, base: &'a [List2<T>]) -> bool {
    match list.next {
        Some(index) => {
            *list = &base[ix(index)];
            true
        }
        None => false,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct List<'list, T> {
    item: T,
    next: Option<&'list Self>,
}

impl<'list, T> List<'list, T> {
    /// Returns the number of elements in the list.
    // Never empty
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        let mut curr = Some(self);
        std::iter::from_fn(move || {
            let next = curr?;
            curr = next.next;
            Some(&next.item)
        })
    }
}

impl<'list, T: fmt::Display> List<'list, T> {
    /// Returns a [`Display`]able type with a provided delimiter.
    pub fn delim<'a>(&'a self, delim: &'a str) -> Delim<'a, T> {
        Delim { list: self, delim }
    }
}

pub struct Delim<'a, T> {
    list: &'a List<'a, T>,
    delim: &'a str,
}

impl<T: fmt::Display> fmt::Display for Delim<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.list.item.fmt(f)?;
        if let Some(remaining) = self.list.next {
            for item in remaining.iter() {
                write!(f, "{}{}", self.delim, item)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Polytype {
    typevars: Vec<Var>,
    typ: Type,
}

impl Polytype {
    pub fn new(ty: Type) -> Self {
        Polytype {
            typevars: vec![],
            typ: ty,
        }
    }
}

pub struct Scope<'outer, 'hir, 'input> {
    env: &'outer mut Env<'input>,
    type_map: &'outer HashMap<&'outer str, Type>,
    errors: &'outer mut Vec<LowerError>,
    original_errors_len: usize,
    globals: &'hir HashMap<&'hir str, Polytype>,
    locals: &'outer mut Vec<(&'hir str, Type)>,
    original_locals_len: usize,
}

impl<'outer, 'hir, 'input: 'hir> Scope<'outer, 'hir, 'input> {
    pub fn new(
        env: &'outer mut Env<'input>,
        type_map: &'outer HashMap<&'outer str, Type>,
        errors: &'outer mut Vec<LowerError>,
        globals: &'hir HashMap<&'hir str, Polytype>,
        locals: &'outer mut Vec<(&'hir str, Type)>,
    ) -> Self {
        let original_errors_len = errors.len();
        let original_locals_len = locals.len();
        Scope {
            env,
            type_map,
            errors,
            original_errors_len,
            globals,
            locals,
            original_locals_len,
        }
    }

    /// Search through local variables first, then search through global variables.
    pub fn type_of(&mut self, var: &str) -> Option<Type> {
        self.locals
            .iter()
            .rev()
            .find_map(|(ident, ty)| (*ident == var).then_some(*ty))
            .or_else(|| {
                self.globals
                    .get(var)
                    .map(|polytype| self.env.monomorphize(polytype))
            })
    }

    pub fn add_local(&mut self, var: &'hir str, ty: Type) {
        self.locals.push((var, ty));
    }

    /// Enter a new scope.
    ///
    /// This method will uniquely borrow a `Bindings` to create another `Bindings`,
    /// which represents an inner scope. When the returned type is dropped,
    /// all bindings that were added in the inner scope will be removed,
    /// leaving the original scope in its initial state and accessible again
    /// since it's no longer borrowed.
    pub fn enter_scope(&mut self) -> Scope<'_, 'hir, 'input> {
        Scope::new(
            self.env,
            self.type_map,
            self.errors,
            self.globals,
            self.locals,
        )
    }

    pub fn had_errors(&self) -> bool {
        self.errors.len() > self.original_errors_len
    }

    pub fn lower(&mut self, expr: &ast::Expr<'_, 'input>) -> Result<Expr<'input>, PushedErrors> {
        match expr {
            ast::Expr::Paren(paren) => self.lower(paren.expr),
            ast::Expr::Symbol(symbol) => match symbol {
                ast::ExprSymbol::Plus(_) => Ok(Expr::Builtin(Builtin::Add)),
                ast::ExprSymbol::Minus(_) => Ok(Expr::Builtin(Builtin::Sub)),
                ast::ExprSymbol::Star(_) => Ok(Expr::Builtin(Builtin::Mul)),
                ast::ExprSymbol::Percent(_) => Ok(Expr::Builtin(Builtin::Rem)),
                ast::ExprSymbol::Slash(_) => Ok(Expr::Builtin(Builtin::Div)),
                ast::ExprSymbol::Dot(_) => todo!("lower `.`"),
                ast::ExprSymbol::DotDot(_) => todo!("lower `..`"),
                ast::ExprSymbol::Semi(_) => todo!("lower `;`"),
                ast::ExprSymbol::Equal(_) => Ok(Expr::Builtin(Builtin::Eq)),
                ast::ExprSymbol::Less(_) => Ok(Expr::Builtin(Builtin::Lt)),
                ast::ExprSymbol::Greater(_) => Ok(Expr::Builtin(Builtin::Gt)),
                ast::ExprSymbol::LessEqual(_) => Ok(Expr::Builtin(Builtin::Le)),
                ast::ExprSymbol::GreaterEqual(_) => Ok(Expr::Builtin(Builtin::Ge)),
            },
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Expr::I32(int)),
                    Err(e) => {
                        self.errors.push(LowerError::ParseInt(e));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    if let Some(ty) = self.type_of(ident.literal) {
                        Ok(Expr::Ident {
                            literal: ident.literal,
                            ty,
                        })
                    } else {
                        self.errors
                            .push(LowerError::IdentNotFound(ident.literal.to_string()));
                        Err(PushedErrors)
                    }
                }
                ast::ExprLit::True(_) => Ok(Expr::Bool(true)),
                ast::ExprLit::False(_) => Ok(Expr::Bool(false)),
            },
            ast::Expr::Tuple(tuple) => {
                fn rec<'ast, 'hir, 'input: 'ast + 'hir>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    mut exprs: impl Iterator<Item = &'ast ast::Expr<'ast, 'input>>,
                ) -> Result<Option<(TupleItemExprIx, TupleItemExprIx)>, PushedErrors>
                {
                    let Some(expr) = exprs.next() else {
                        return Ok(None);
                    };

                    let expr = scope.lower(expr)?;

                    let Some((next_expr, next_type)) = rec(scope, exprs)? else {
                        return Ok(None);
                    };

                    Ok(Some((
                        scope.env.push_tuple_item_exprs(List2 {
                            item: expr,
                            next: Some(next_expr),
                        }),
                        scope.env.push_tuple_item_types(List2 {
                            item: expr.ty(),
                            next: Some(next_type),
                        }),
                    )))
                }

                if let Some((exprs, ty)) = rec(self, tuple.iter_elements().copied())? {
                    Ok(Expr::Tuple {
                        ty: Type::Tuple(ty),
                        exprs,
                    })
                } else {
                    Ok(Expr::Unit)
                }
            }
            ast::Expr::Closure(closure) => {
                let mut inner = self.enter_scope();

                // Need to parse the params _before_ the body...
                // Duh.
                let (lhs, rhs) = inner.type_of_many_params(&closure.head.params)?;
                let body = inner.lower(closure.head.body)?;

                drop(inner);

                fn rec<'ast, 'hir, 'input: 'ast + 'hir>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    head: &ExprBranch<'input>,
                    mut branches: impl Iterator<Item = &'ast ast::ExprBranch<'ast, 'input>>,
                ) -> Result<Option<ExprBranchIx>, PushedErrors> {
                    let Some(branch) = branches.next() else {
                        return Ok(None);
                    };

                    let mut inner = scope.enter_scope();
                    let (lhs, rhs) = inner.type_of_many_params(&branch.params)?;
                    let body = inner.lower(branch.body)?;
                    drop(inner);

                    scope.unify(head.lhs.ty(), lhs.ty());
                    scope.unify(head.rhs.ty(), rhs.ty());
                    scope.unify(head.body.ty(), body.ty());

                    if scope.had_errors() {
                        Err(PushedErrors)
                    } else {
                        let next = rec(scope, head, branches)?;
                        Ok(Some(scope.env.push_expr_branches(List2 {
                            item: ExprBranch { lhs, rhs, body },
                            next,
                        })))
                    }
                }

                let head = ExprBranch { lhs, rhs, body };

                let next = rec(self, &head, closure.tail.iter().map(|(_, branch)| branch))?;

                let branches = self.env.push_expr_branches(List2 { item: head, next });

                let ty = Type::Function(self.env.push_type_function(BoxedTypeFunction {
                    lhs: head.lhs.ty(),
                    rhs: head.rhs.ty(),
                    output: head.body.ty(),
                }));

                Ok(Expr::Closure { ty, branches })
            }
            ast::Expr::Appl(appl) => {
                let lhs = self.lower(appl.lhs);
                let rhs = self.lower(appl.rhs);
                let function = self.lower(appl.function);

                let (Ok(lhs), Ok(rhs), Ok(function)) = (lhs, rhs, function) else {
                    return Err(PushedErrors);
                };

                let ty = self.env.new_typevar().1;

                let index_of_function = self.env.push_type_function(BoxedTypeFunction {
                    lhs: lhs.ty(),
                    rhs: rhs.ty(),
                    output: ty,
                });

                self.unify(function.ty(), Type::Function(index_of_function));

                if self.had_errors() {
                    Err(PushedErrors)
                } else {
                    Ok(Expr::Appl {
                        ty,
                        appl: self
                            .env
                            .push_expr_appl(BoxedExprAppl { lhs, function, rhs }),
                    })
                }
            }
        }
    }

    /// Returns the [`Type`] of an [`ast::ExprPat`].
    fn lower_pat(&mut self, pat: &ast::ExprPat<'_, 'input>) -> Result<Pat<'input>, PushedErrors> {
        match pat {
            ast::Pat::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Pat::I32(int)),
                    Err(e) => {
                        self.errors.push(LowerError::ParseInt(e));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    let ty = self.env.new_typevar().1;
                    self.add_local(ident.literal, ty);
                    Ok(Pat::Ident {
                        literal: ident.literal,
                        ty,
                    })
                }
                ast::ExprLit::True(_) => Ok(Pat::Bool(true)),
                ast::ExprLit::False(_) => Ok(Pat::Bool(false)),
            },
            ast::Pat::Tuple(tuple) => {
                // TODO(quinn): this is mostly copy pasted from `Env::lower`,
                // can we try to generalize them? Patterns are basically just
                // expressions without closures or function application...
                fn rec<'ast, 'hir, 'input: 'ast + 'hir>(
                    bindings: &mut Scope<'_, 'hir, 'input>,
                    mut pats: impl Iterator<Item = &'ast ast::ExprPat<'ast, 'input>>,
                ) -> Result<Option<(TupleItemExprPatIx, TupleItemTypeIx)>, PushedErrors>
                {
                    let Some(pat) = pats.next() else {
                        return Ok(None);
                    };

                    let pat = bindings.lower_pat(pat)?;

                    let Some((next_pat, next_type)) = rec(bindings, pats)? else {
                        return Ok(None);
                    };

                    Ok(Some((
                        bindings.env.push_tuple_item_expr_pats(List2 {
                            item: pat,
                            next: Some(next_pat),
                        }),
                        bindings.env.push_tuple_item_types(List2 {
                            item: pat.ty(),
                            next: Some(next_type),
                        }),
                    )))
                }

                if let Some((exprs, ty)) = rec(self, tuple.iter_elements().copied())? {
                    Ok(Pat::Tuple {
                        ty: Type::Tuple(ty),
                        exprs,
                    })
                } else {
                    Ok(Pat::Unit)
                }
            } // When we add struct destructuring, we can unify the type of the field
              // with the returned type of the pattern in that field.
              // e.g. If we have a `struct Number(i32)` and have the pattern
              // `Number(x)`, then `x` might be type `Var(Typevar::Unbound(0))` which
              // we can unify with `i32` to see that it should be `Var(Typevar::Bound(&Int))`
        }
    }

    /// Returns the [`Type`] of a single [`ast::ExprParam`].
    fn lower_param(
        &mut self,
        param: &ast::ExprParam<'_, 'input>,
    ) -> Result<Pat<'input>, PushedErrors> {
        let pat_type = self.lower_pat(param.pat)?;
        if let Some((_, annotation)) = param.ty {
            let t2 = self.env.type_from_ast(annotation, self.type_map);
            self.unify(pat_type.ty(), t2);
            if self.had_errors() {
                return Err(PushedErrors);
            }
        }

        Ok(pat_type)
    }

    /// Returns the [`Type`]s of various [`ast::ExprParams`].
    fn type_of_many_params(
        &mut self,
        params: &ast::ExprParams<'_, 'input>,
    ) -> Result<(Pat<'input>, Pat<'input>), PushedErrors> {
        match params {
            ast::ExprParams::Zero => Ok((Pat::Unit, Pat::Unit)),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(lhs);
                Ok((lhs_type?, Pat::Unit))
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(lhs);
                let rhs_type = self.lower_param(rhs);
                Ok((lhs_type?, rhs_type?))
            }
        }
    }

    /// Unify two types.
    fn unify(&mut self, t1: Type, t2: Type) -> NodeIndex {
        match (t1, t2) {
            (Type::I32, Type::I32) => self.env.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Bool, Type::Bool) => self.env.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Unit, Type::Unit) => self.env.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Tuple(a), Type::Tuple(b)) => {
                let base = &self.env.tuple_item_types_vec[..];
                let mut a = base[ix(a)];
                let mut b = base[ix(b)];
                if a.len(base) == b.len(base) {
                    let conclusion = self.env.equations.add_rule(Node::Equiv(t1, t2));

                    for i in 0.. {
                        let t1 = a.item;
                        let t2 = b.item;
                        let mut inner = self.enter_scope();
                        let proof = inner.unify(t1, t2);
                        if inner.had_errors() {
                            inner.env.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                        }
                        inner
                            .env
                            .equations
                            .add_proof(proof, conclusion, Edge::Tuple(i));
                        drop(inner);

                        let base = &self.env.tuple_item_types_vec[..];

                        if let (Some(next_a), Some(next_b)) = (a.next, b.next) {
                            a = base[ix(next_a)];
                            b = base[ix(next_b)];
                        } else {
                            break;
                        }
                    }
                    // do this weird dance to avoid holding a borrow to base
                    // while let (Some(l1), Some(l2)) = (a.next(&self.env.tuple_item_types_vec[..]), b.next(&self.env.tuple_item_types_vec[..])) {
                    //     a = l1;
                    //     b = l2;
                    //     let (t1, t2) = (l1.item, l2.item);
                    //     drop(l1);
                    //     drop(l2);

                    //     let mut inner = self.enter_scope();
                    //     let proof = inner.unify(t1, t2);
                    //     if inner.had_errors() {
                    //         inner.env.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                    //     }
                    //     inner
                    //         .env
                    //         .equations
                    //         .add_proof(proof, conclusion, Edge::Tuple(i));
                    //     i += 1;
                    // }

                    // for (i, (&t1, &t2)) in a.iter(base).zip(b.iter(base)).enumerate() {
                    //     let mut inner = self.enter_scope();
                    //     let proof = inner.unify(t1, t2);
                    //     if inner.had_errors() {
                    //         inner.env.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                    //     }
                    //     inner
                    //         .env
                    //         .equations
                    //         .add_proof(proof, conclusion, Edge::Tuple(i));
                    // }
                    conclusion
                } else {
                    // different length tuples
                    self.errors.push(LowerError::Unify(t1, t2));
                    self.env.equations.add_rule(Node::NotEquiv(t1, t2))
                }
            }
            (Type::Var(var), a) | (a, Type::Var(var)) => {
                if let Some((b, _binding_source)) = self.env.typevars[ix(var.0)] {
                    let proof = self.unify(a, b);

                    let conclusion = if self.had_errors() {
                        self.env.equations.add_rule(Node::NotEquiv(t1, t2))
                    } else {
                        self.env.equations.add_rule(Node::Equiv(t1, t2))
                    };
                    // If we wanted, we could also add an edge with `_binding_source`,
                    // which tells us exactly where the typevar was bound.
                    self.env
                        .equations
                        .add_proof(proof, conclusion, Edge::Transitivity);
                    conclusion
                } else if t1 == t2 {
                    self.env.equations.add_rule(Node::Equiv(t1, t2))
                } else if occurs(&self.env.typevars, var, a, &self.env.type_functions_vec) {
                    self.errors.push(LowerError::CyclicType(var, a));
                    self.env.equations.add_rule(Node::NotEquiv(t1, t2))
                } else {
                    // The actual binding code is here
                    let conclusion = self
                        .env
                        .equations
                        .add_rule(Node::Binding { var, definition: a });

                    self.env.typevars[ix(var.0)] = Some((a, conclusion));
                    conclusion
                }
            }
            (Type::Function(f1), Type::Function(f2)) => {
                let base = &self.env.type_functions_vec[..];
                let f1 = base[ix(f1)];
                let f2 = base[ix(f2)];

                let conclusion = self.env.equations.add_rule(Node::Equiv(t1, t2));
                let lhs_proof = self.unify(f1.lhs, f2.lhs);
                let rhs_proof = self.unify(f1.rhs, f2.rhs);
                let output_proof = self.unify(f1.output, f2.output);

                if self.had_errors() {
                    self.env.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                }

                self.env
                    .equations
                    .add_proof(lhs_proof, conclusion, Edge::FunctionLhs);
                self.env
                    .equations
                    .add_proof(rhs_proof, conclusion, Edge::FunctionRhs);
                self.env
                    .equations
                    .add_proof(output_proof, conclusion, Edge::FunctionOutput);

                conclusion
            }
            _ => {
                self.errors.push(LowerError::Unify(t1, t2));
                self.env.equations.add_rule(Node::NotEquiv(t1, t2))
            }
        }
    }
}

impl Drop for Scope<'_, '_, '_> {
    fn drop(&mut self) {
        self.locals.truncate(self.original_locals_len);
    }
}

#[derive(Clone, Debug, Error, PartialEq)]
pub enum LowerError {
    #[error("Cannot unify types")]
    Unify(Type, Type),
    #[error("Cyclic type")]
    CyclicType(Var, Type),
    #[error("Identifier not found: `{0}`")]
    IdentNotFound(String),
    #[error(transparent)]
    ParseInt(num::ParseIntError),
}

/// Count the number of allocations in an [`ast::Program<'_, '_>`].
#[derive(Debug)]
pub struct AllocationCounter {
    pub type_functions: usize,
    pub expr_appls: usize,
    pub expr_branches: usize,
    pub tuple_item_exprs: usize,
    pub tuple_item_types: usize,
    pub tuple_item_expr_pats: usize,
}

impl AllocationCounter {
    pub fn count_in_program(program: &ast::Program<'_, '_>) -> Self {
        const TYPE_FUNCTIONS_ALLOCATED_IN_DEFAULT_GLOBALS: usize = 3;
        const GENERIC_FNS_INSTANTIATED_GUESS: usize = 64;

        let mut counter = AllocationCounter {
            type_functions: TYPE_FUNCTIONS_ALLOCATED_IN_DEFAULT_GLOBALS
                + GENERIC_FNS_INSTANTIATED_GUESS,
            expr_appls: 0,
            expr_branches: 0,
            tuple_item_exprs: 0,
            tuple_item_types: 0,
            tuple_item_expr_pats: 0,
        };

        for item in program.items.iter() {
            counter.count_in_expr(item.expr);
        }

        counter
    }

    fn count_in_expr(&mut self, expr: &ast::Expr<'_, '_>) {
        match expr {
            ast::Expr::Paren(parens) => self.count_in_expr(parens.expr),
            ast::Expr::Tuple(tuple) => {
                self.tuple_item_exprs += tuple.len();
                // self.tuple_item_expr_pats += tuple.len();
                for element in tuple.iter_elements() {
                    self.count_in_expr(element);
                }
            }
            ast::Expr::Closure(closure) => {
                self.expr_branches += closure.num_branches();
                self.type_functions += 1;
                for branch in closure.iter_branches() {
                    fn pat_rec(
                        counter: &mut AllocationCounter,
                        pat: &ast::Pat<'_, ast::ExprLit<'_>>,
                    ) {
                        if let ast::Pat::Tuple(tuple) = pat {
                            counter.tuple_item_expr_pats += tuple.len();
                            counter.tuple_item_types += tuple.len();
                            for pat in tuple.iter_elements() {
                                pat_rec(counter, pat);
                            }
                        }
                    }

                    fn type_from_ast_rec(counter: &mut AllocationCounter, ty: &ast::Type<'_, '_>) {
                        match ty {
                            ast::Type::Tuple(tuple) => {
                                counter.tuple_item_types += tuple.len();
                                for element in tuple.iter_elements() {
                                    type_from_ast_rec(counter, element);
                                }
                            }
                            ast::Type::Function(function) => {
                                counter.type_functions += 1;
                                type_from_ast_rec(counter, function.lhs);
                                type_from_ast_rec(counter, function.rhs);
                                type_from_ast_rec(counter, function.ret);
                            }
                            _ => {}
                        }
                    }

                    match &branch.params {
                        ast::ExprParams::Zero => {}
                        ast::ExprParams::One(p1) => {
                            pat_rec(self, p1.pat);
                            if let Some((_, ty)) = p1.ty {
                                type_from_ast_rec(self, ty);
                            }
                        }
                        ast::ExprParams::Two(p1, _, p2) => {
                            pat_rec(self, p1.pat);
                            pat_rec(self, p2.pat);
                            if let Some((_, ty)) = p1.ty {
                                type_from_ast_rec(self, ty);
                            }
                            if let Some((_, ty)) = p2.ty {
                                type_from_ast_rec(self, ty);
                            }
                        }
                    };

                    self.count_in_expr(branch.body);
                }
            }
            ast::Expr::Appl(appl) => {
                self.expr_appls += 1; // new
                self.type_functions += 1;
                self.count_in_expr(appl.lhs);
                self.count_in_expr(appl.function);
                self.count_in_expr(appl.rhs);
            }
            ast::Expr::Symbol(_) => {}
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Integer(_) | ast::ExprLit::Ident(_) => {}
                _ => {}
            },
        }
    }
}

pub struct Env<'input> {
    type_functions_vec: Vec<BoxedTypeFunction>,
    expr_appls_vec: Vec<BoxedExprAppl<'input>>,
    tuple_item_exprs_vec: Vec<List2<Expr<'input>>>,
    tuple_item_types_vec: Vec<List2<Type>>,
    tuple_item_expr_pats_vec: Vec<List2<Pat<'input>>>,
    expr_branches_vec: Vec<List2<ExprBranch<'input>>>,
    typevars: Vec<Typevar>,
    equations: Equations,
}

impl<'input> Env<'input> {
    pub fn new() -> Self {
        Env {
            typevars: vec![],
            equations: Equations::new(),
            type_functions_vec: vec![
                // For `Builtin`s impl of the `Ty` trait.
                BoxedTypeFunction {
                    lhs: Type::I32,
                    rhs: Type::I32,
                    output: Type::I32,
                },
                BoxedTypeFunction {
                    lhs: Type::I32,
                    rhs: Type::I32,
                    output: Type::Bool,
                },
            ],
            expr_appls_vec: vec![],
            tuple_item_exprs_vec: vec![],
            tuple_item_types_vec: vec![],
            tuple_item_expr_pats_vec: vec![],
            expr_branches_vec: vec![],
        }
    }

    pub fn new_typevar(&mut self) -> (Var, Type) {
        let var_ptr = Var(self.typevars.len() as _);
        self.typevars.push(None);
        (var_ptr, Type::Var(var_ptr))
    }

    /// Get a global environment with the type signatures of `in` and `print`
    /// already loaded in.
    ///
    /// `in`: `x (x () -> y) -> y`
    ///
    /// and
    ///
    /// `print`: `x () -> ()`
    pub fn default_globals(&mut self) -> impl Iterator<Item = (&'static str, Polytype)> {
        [
            ("in", {
                let (x, x_type) = self.new_typevar();
                let (y, y_type) = self.new_typevar();

                let rhs = Type::Function(self.push_type_function(BoxedTypeFunction {
                    lhs: x_type,
                    rhs: Type::Unit,
                    output: y_type,
                }));

                Polytype {
                    typevars: vec![x, y],
                    typ: Type::Function(self.push_type_function(BoxedTypeFunction {
                        lhs: x_type,
                        rhs,
                        output: y_type,
                    })),
                }
            }),
            ("print", {
                let (x, x_type) = self.new_typevar();

                Polytype {
                    typevars: vec![x],
                    typ: Type::Function(self.push_type_function(BoxedTypeFunction {
                        lhs: x_type,
                        rhs: Type::Unit,
                        output: Type::Unit,
                    })),
                }
            }),
        ]
        .into_iter()
    }

    pub fn monomorphize(&mut self, polytype: &Polytype) -> Type {
        // Takes a polymorphic type and replaces all instances of generics
        // with a fixed, unbound type.
        // For example, id: T -> T is a polymorphic type, so it goes through
        // and replaces both `T`s with an unbound type variable like `a0`,
        // which is then bound later on.
        fn replace_unbound_typevars(tbl: &HashMap<Var, Type>, env: &mut Env<'_>, ty: Type) -> Type {
            match ty {
                Type::Var(var) => {
                    if let Some((t, _)) = env.typevars[ix(var.0)] {
                        replace_unbound_typevars(tbl, env, t)
                    } else if let Some(&t) = tbl.get(&var) {
                        t
                    } else {
                        ty
                    }
                }
                Type::Function(boxed) => {
                    let boxed = env.type_functions_vec[ix(boxed)];
                    let boxed = BoxedTypeFunction {
                        lhs: replace_unbound_typevars(tbl, env, boxed.lhs),
                        rhs: replace_unbound_typevars(tbl, env, boxed.rhs),
                        output: replace_unbound_typevars(tbl, env, boxed.output),
                    };
                    Type::Function(env.push_type_function(boxed))
                }
                _ => ty,
            }
        }

        let tvs_to_replace = polytype
            .typevars
            .iter()
            .map(|tv| (*tv, self.new_typevar().1))
            .collect();

        replace_unbound_typevars(&tvs_to_replace, self, polytype.typ)
    }

    pub fn push_type_function(&mut self, boxed: BoxedTypeFunction) -> BoxedTypeFunctionIx {
        let ix = self.type_functions_vec.len() as _;
        self.type_functions_vec.push(boxed);
        ix
    }

    pub fn push_expr_appl(&mut self, boxed: BoxedExprAppl<'input>) -> BoxedExprApplIx {
        let ix = self.expr_appls_vec.len() as _;
        self.expr_appls_vec.push(boxed);
        ix
    }

    pub fn push_tuple_item_exprs(&mut self, boxed: List2<Expr<'input>>) -> TupleItemExprIx {
        let ix = self.tuple_item_exprs_vec.len() as _;
        self.tuple_item_exprs_vec.push(boxed);
        ix
    }

    pub fn push_tuple_item_types(&mut self, boxed: List2<Type>) -> TupleItemTypeIx {
        let ix = self.tuple_item_types_vec.len() as _;
        self.tuple_item_types_vec.push(boxed);
        ix
    }

    pub fn push_tuple_item_expr_pats(&mut self, boxed: List2<Pat<'input>>) -> TupleItemExprPatIx {
        let ix = self.tuple_item_expr_pats_vec.len() as _;
        self.tuple_item_expr_pats_vec.push(boxed);
        ix
    }

    pub fn push_expr_branches(&mut self, boxed: List2<ExprBranch<'input>>) -> ExprBranchIx {
        let ix = self.expr_branches_vec.len() as _;
        self.expr_branches_vec.push(boxed);
        ix
    }

    /// Convert an [`ast::Type`] annotation into an HIR [`Type`].
    pub fn type_from_ast(
        &mut self,
        typ: &ast::Type<'_, 'input>,
        map: &HashMap<&str, Type>,
    ) -> Type {
        match typ {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => Type::I32,
                "bool" => Type::Bool,
                other => map.get(other).copied().expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                // Build up the linked list of types from the inside out
                fn rec<'ast, 'input: 'ast>(
                    env: &mut Env<'input>,
                    map: &HashMap<&str, Type>,
                    mut types: impl Iterator<Item = &'ast ast::Type<'ast, 'input>>,
                ) -> Option<TupleItemExprIx> {
                    let item = env.type_from_ast(types.next()?, map);
                    let next = rec(env, map, types);
                    Some(env.push_tuple_item_types(List2 { item, next }))
                }

                if let Some(ty) = rec(self, map, tuple.iter_elements().copied()) {
                    Type::Tuple(ty)
                } else {
                    Type::Unit
                }
            }
            ast::Type::Function(function) => {
                let lhs = self.type_from_ast(function.lhs, map);
                let rhs = self.type_from_ast(function.rhs, map);
                let output = self.type_from_ast(function.ret, map);

                Type::Function(self.push_type_function(BoxedTypeFunction { lhs, rhs, output }))
            }
        }
    }
}

#[derive(Debug)]
pub struct PushedErrors;

fn occurs(typevars: &[Typevar], var: Var, ty: Type, base: &[BoxedTypeFunction]) -> bool {
    match ty {
        Type::Var(typevar) => {
            if let Some((t, _)) = typevars[ix(typevar.0)] {
                occurs(typevars, var, t, base)
            } else {
                var == typevar
            }
        }
        Type::Function(index) => {
            let BoxedTypeFunction { lhs, rhs, output } = base[ix(index)];
            occurs(typevars, var, lhs, base)
                || occurs(typevars, var, rhs, base)
                || occurs(typevars, var, output, base)
        }
        _ => false,
    }
}
