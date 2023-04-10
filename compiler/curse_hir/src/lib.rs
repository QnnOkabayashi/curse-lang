#![allow(dead_code)]
use curse_arena::Arena;
use curse_ast as ast;
use displaydoc::Display;
use expr::*;
use petgraph::graph::NodeIndex;
use std::{collections::HashMap, fmt, num};
use thiserror::Error;

mod equations;
use equations::{Edge, Equations, Node};
mod expr;

#[cfg(test)]
mod tests;

/// `Some` is bound, `None` is unbound.
pub type Typevar<'hir> = Option<(&'hir Type<'hir>, NodeIndex)>;

/// Newtype around a `usize` used to index into the `typevars` field of `Env`.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("T{0}")]
pub struct Var(usize);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type<'hir> {
    I32,
    Bool,
    Unit,
    Tuple(&'hir List<'hir, &'hir Self>),
    Var(Var),
    Function(TypeFunction<&'hir Self>),
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Tuple(elements) => {
                write!(f, "(")?;
                write!(f, "{}", elements.item)?;
                if let Some(remaining) = elements.next {
                    for item in remaining.iter() {
                        write!(f, ", {item}")?;
                    }
                }
                write!(f, ")")
            }
            Type::Var(var) => write!(f, "{var}"),
            Type::Function(fun) => write!(f, "{fun}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct List<'list, T> {
    // doesn't take a ref because for closure branches, we don't want to
    // have to put them in another arena
    item: T,
    next: Option<&'list Self>,
}

impl<'list, T> List<'list, T> {
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TypeFunction<Ty> {
    lhs: Ty,
    rhs: Ty,
    output: Ty,
}

impl<Ty: fmt::Display> fmt::Display for TypeFunction<Ty> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} -> {})", self.lhs, self.rhs, self.output)
    }
}

#[derive(Clone)]
pub struct Polytype<'hir> {
    typevars: Vec<Var>,
    typ: &'hir Type<'hir>,
}

impl<'hir> Polytype<'hir> {
    pub fn new(ty: &'hir Type<'hir>) -> Self {
        Polytype {
            typevars: vec![],
            typ: ty,
        }
    }
}

pub struct Scope<'outer, 'hir, 'input> {
    env: &'outer mut Env<'hir, 'input>,
    type_map: &'outer HashMap<&'outer str, &'hir Type<'hir>>,
    errors: &'outer mut Vec<LowerError<'hir>>,
    original_errors_len: usize,
    globals: &'hir HashMap<&'hir str, Polytype<'hir>>,
    locals: &'outer mut Vec<(&'hir str, &'hir Type<'hir>)>,
    original_locals_len: usize,
}

impl<'outer, 'hir, 'input> Scope<'outer, 'hir, 'input> {
    pub fn new(
        env: &'outer mut Env<'hir, 'input>,
        type_map: &'outer HashMap<&'outer str, &'hir Type<'hir>>,
        errors: &'outer mut Vec<LowerError<'hir>>,
        globals: &'hir HashMap<&'hir str, Polytype<'hir>>,
        locals: &'outer mut Vec<(&'hir str, &'hir Type<'hir>)>,
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
    pub fn get(&mut self, var: &str) -> Option<&'hir Type<'hir>> {
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

    pub fn add_local(&mut self, var: &'hir str, ty: &'hir Type<'hir>) {
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

    pub fn lower(
        &mut self,
        expr: &ast::Expr<'_, 'input>,
    ) -> Result<&'hir Expr<'hir, 'input>, PushedErrors> {
        match expr {
            ast::Expr::Paren(paren) => self.lower(paren.expr),
            ast::Expr::Symbol(symbol) => match symbol {
                ast::ExprSymbol::Plus(_) => Ok(&Expr::Builtin(Builtin::Add)),
                ast::ExprSymbol::Minus(_) => Ok(&Expr::Builtin(Builtin::Sub)),
                ast::ExprSymbol::Star(_) => Ok(&Expr::Builtin(Builtin::Mul)),
                ast::ExprSymbol::Percent(_) => Ok(&Expr::Builtin(Builtin::Rem)),
                ast::ExprSymbol::Slash(_) => Ok(&Expr::Builtin(Builtin::Div)),
                ast::ExprSymbol::Dot(_) => todo!("lower `.`"),
                ast::ExprSymbol::DotDot(_) => todo!("lower `..`"),
                ast::ExprSymbol::Semi(_) => todo!("lower `;`"),
                ast::ExprSymbol::Equal(_) => Ok(&Expr::Builtin(Builtin::Eq)),
                ast::ExprSymbol::Less(_) => Ok(&Expr::Builtin(Builtin::Lt)),
                ast::ExprSymbol::Greater(_) => Ok(&Expr::Builtin(Builtin::Gt)),
                ast::ExprSymbol::LessEqual(_) => Ok(&Expr::Builtin(Builtin::Le)),
                ast::ExprSymbol::GreaterEqual(_) => Ok(&Expr::Builtin(Builtin::Ge)),
            },
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(self.env.exprs.push(Expr::I32(int))),
                    Err(e) => {
                        self.errors.push(LowerError::ParseInt(e));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    if let Some(ty) = self.get(ident.literal) {
                        Ok(self.env.exprs.push(Expr::Ident(ExprIdent {
                            literal: ident.literal,
                            ty,
                        })))
                    } else {
                        self.errors
                            .push(LowerError::IdentNotFound(ident.literal.to_string()));
                        Err(PushedErrors)
                    }
                }
                ast::ExprLit::True(_) => Ok(&Expr::Bool(true)),
                ast::ExprLit::False(_) => Ok(&Expr::Bool(false)),
            },
            ast::Expr::Tuple(tuple) => {
                // tuple rec fn
                fn rec<'ast, 'hir, 'input: 'ast>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    mut exprs: impl Iterator<Item = &'ast ast::Expr<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir List<'hir, &'hir Expr<'hir, 'input>>,
                        &'hir List<'hir, &'hir Type<'hir>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(expr) = exprs.next() else {
                        return Ok(None);
                    };

                    let expr = scope.lower(expr)?;

                    let Some((next_expr, next_type)) = rec(scope, exprs)? else {
                        return Ok(None);
                    };

                    Ok(Some((
                        scope.env.tuple_item_exprs.push(List {
                            item: expr,
                            next: Some(next_expr),
                        }),
                        scope.env.tuple_item_types.push(List {
                            item: expr.ty(),
                            next: Some(next_type),
                        }),
                    )))
                }

                if let Some((exprs, ty)) = rec(self, tuple.iter_elements().copied())? {
                    Ok(self.env.exprs.push(Expr::Tuple(ExprTuple {
                        ty: self.env.types.push(Type::Tuple(ty)),
                        exprs,
                    })))
                } else {
                    Ok(&Expr::Unit)
                }
            }
            ast::Expr::Closure(closure) => {
                let mut inner_scope = self.enter_scope();

                // Need to parse the params _before_ the body...
                // Duh.
                let (lhs, rhs) = inner_scope.type_of_many_params(&closure.head.params)?;
                let body = inner_scope.lower(closure.head.body)?;

                drop(inner_scope);

                fn rec<'ast, 'hir, 'input: 'ast>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    head: &ExprBranch<'hir, 'input>,
                    mut branches: impl Iterator<Item = &'ast ast::ExprBranch<'ast, 'input>>,
                ) -> Result<Option<&'hir List<'hir, ExprBranch<'hir, 'input>>>, PushedErrors>
                {
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
                        Ok(Some(scope.env.expr_branches.push(List {
                            item: ExprBranch { lhs, rhs, body },
                            next: rec(scope, head, branches)?,
                        })))
                    }
                }

                let head = ExprBranch { lhs, rhs, body };

                let next = rec(self, &head, closure.tail.iter().map(|(_, branch)| branch))?;

                let branches = self.env.expr_branches.push(List { item: head, next });

                let ty = self.env.types.push(Type::Function(TypeFunction {
                    lhs: head.lhs.ty(),
                    rhs: head.rhs.ty(),
                    output: head.body.ty(),
                }));

                Ok(self
                    .env
                    .exprs
                    .push(Expr::Closure(ExprClosure { ty, branches })))
            }
            ast::Expr::Appl(appl) => {
                let lhs = self.lower(appl.lhs);
                let rhs = self.lower(appl.rhs);
                let function = self.lower(appl.function);

                let (Ok(lhs), Ok(rhs), Ok(function)) = (lhs, rhs, function) else {
                    return Err(PushedErrors);
                };

                let ty = self.env.new_typevar().1;

                self.unify(
                    function.ty(),
                    self.env.types.push(Type::Function(TypeFunction {
                        lhs: lhs.ty(),
                        rhs: rhs.ty(),
                        output: ty,
                    })),
                );

                if self.had_errors() {
                    Err(PushedErrors)
                } else {
                    Ok(self.env.exprs.push(Expr::Appl(ExprAppl {
                        ty,
                        lhs,
                        function,
                        rhs,
                    })))
                }
            }
        }
    }

    /// Returns the [`Type`] of an [`ast::ExprPat`].
    fn lower_pat(
        &mut self,
        pat: &ast::ExprPat<'_, 'input>,
    ) -> Result<&'hir ExprPat<'hir, 'input>, PushedErrors> {
        match pat {
            ast::Pat::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(self.env.expr_pats.push(ExprPat::I32(int))),
                    Err(e) => {
                        self.errors.push(LowerError::ParseInt(e));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    let ty = self.env.new_typevar().1;
                    self.add_local(ident.literal, ty);
                    Ok(self.env.expr_pats.push(ExprPat::Ident(ExprIdent {
                        literal: ident.literal,
                        ty,
                    })))
                }
                ast::ExprLit::True(_) => Ok(&ExprPat::Bool(true)),
                ast::ExprLit::False(_) => Ok(&ExprPat::Bool(false)),
            },
            ast::Pat::Tuple(tuple) => {
                // TODO(quinn): this is mostly copy pasted from `Env::lower`,
                // can we try to generalize them? Patterns are basically just
                // expressions without closures or function application...
                fn rec<'ast, 'hir, 'input: 'ast>(
                    bindings: &mut Scope<'_, 'hir, 'input>,
                    mut pats: impl Iterator<Item = &'ast ast::ExprPat<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir List<'hir, &'hir ExprPat<'hir, 'input>>,
                        &'hir List<'hir, &'hir Type<'hir>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(pat) = pats.next() else {
                        return Ok(None);
                    };

                    let pat = bindings.lower_pat(pat)?;

                    let Some((next_pat, next_type)) = rec(bindings, pats)? else {
                        return Ok(None);
                    };

                    Ok(Some((
                        bindings.env.tuple_item_expr_pats.push(List {
                            item: pat,
                            next: Some(next_pat),
                        }),
                        bindings.env.tuple_item_types.push(List {
                            item: pat.ty(),
                            next: Some(next_type),
                        }),
                    )))
                }

                if let Some((exprs, ty)) = rec(self, tuple.iter_elements().copied())? {
                    Ok(self.env.expr_pats.push(ExprPat::Tuple(ExprTuple {
                        ty: self.env.types.push(Type::Tuple(ty)),
                        exprs,
                    })))
                } else {
                    Ok(&ExprPat::Unit)
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
    ) -> Result<&'hir ExprPat<'hir, 'input>, PushedErrors> {
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
    ) -> Result<(&'hir ExprPat<'hir, 'input>, &'hir ExprPat<'hir, 'input>), PushedErrors> {
        match params {
            ast::ExprParams::Zero => Ok((&ExprPat::Unit, &ExprPat::Unit)),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(lhs);
                Ok((lhs_type?, &ExprPat::Unit))
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(lhs);
                let rhs_type = self.lower_param(rhs);
                Ok((lhs_type?, rhs_type?))
            }
        }
    }

    /// Convert an [`ast::Type`] annotation into an HIR [`Type`].
    pub fn type_from_ast(&mut self, typ: &ast::Type<'_, 'input>) -> &'hir Type<'hir> {
        match typ {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => &Type::I32,
                "bool" => &Type::Bool,
                other => self.type_map.get(other).expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                // Build up the linked list of types from the inside out
                fn rec<'ast, 'hir, 'input: 'ast>(
                    env: &mut Scope<'_, 'hir, 'input>,
                    mut types: impl Iterator<Item = &'ast ast::Type<'ast, 'input>>,
                ) -> Option<&'hir List<'hir, &'hir Type<'hir>>> {
                    let item = env.type_from_ast(types.next()?);
                    Some(env.env.tuple_item_types.push(List {
                        item,
                        next: rec(env, types),
                    }))
                }

                if let Some(ty) = rec(self, tuple.iter_elements().copied()) {
                    self.env.types.push(Type::Tuple(ty))
                } else {
                    &Type::Unit
                }
            }
            ast::Type::Function(function) => self.env.types.push(Type::Function(TypeFunction {
                lhs: self.type_from_ast(function.lhs),
                rhs: self.type_from_ast(function.rhs),
                output: self.type_from_ast(function.ret),
            })),
        }
    }

    /// Unify two types.
    fn unify(&mut self, t1: &'hir Type<'hir>, t2: &'hir Type<'hir>) -> NodeIndex {
        match (t1, t2) {
            (Type::I32, Type::I32) => self.env.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Bool, Type::Bool) => self.env.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Unit, Type::Unit) => self.env.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Tuple(a), Type::Tuple(b)) => {
                if a.len() == b.len() {
                    let conclusion = self.env.equations.add_rule(Node::Equiv(t1, t2));
                    for (i, (a, b)) in a.iter().zip(b.iter()).enumerate() {
                        let mut inner = self.enter_scope();
                        let proof = inner.unify(a, b);
                        if inner.had_errors() {
                            inner.env.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                        }
                        inner
                            .env
                            .equations
                            .add_proof(proof, conclusion, Edge::Tuple(i));
                    }
                    conclusion
                } else {
                    // different length tuples
                    self.errors.push(LowerError::Unify(t1, t2));
                    self.env.equations.add_rule(Node::NotEquiv(t1, t2))
                }
            }
            (Type::Var(var), a) | (a, Type::Var(var)) => {
                if let Some((b, _binding_source)) = self.env.typevars[var.0] {
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
                } else if occurs(self.env.typevars, *var, a) {
                    self.errors.push(LowerError::CyclicType(*var, a));
                    self.env.equations.add_rule(Node::NotEquiv(t1, t2))
                } else {
                    // The actual binding code is here
                    let conclusion = self.env.equations.add_rule(Node::Binding {
                        var: *var,
                        definition: a,
                    });

                    self.env.typevars[var.0] = Some((a, conclusion));
                    conclusion
                }
            }
            (Type::Function(f1), Type::Function(f2)) => {
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
pub enum LowerError<'hir> {
    #[error("Cannot unify types")]
    Unify(&'hir Type<'hir>, &'hir Type<'hir>),
    #[error("Cyclic type")]
    CyclicType(Var, &'hir Type<'hir>),
    #[error("Identifier not found: `{0}`")]
    IdentNotFound(String),
    #[error(transparent)]
    ParseInt(num::ParseIntError),
}

/// Count the number of allocations in an [`ast::Program<'_, '_>`].
#[derive(Debug)]
pub struct AllocationCounter {
    pub num_exprs: usize,
    pub num_tuple_item_exprs: usize,
    pub num_expr_pats: usize,
    pub num_branches: usize,
}

impl AllocationCounter {
    pub fn count_in_program(program: &ast::Program<'_, '_>) -> Self {
        let mut counter = AllocationCounter {
            num_exprs: 0,
            num_tuple_item_exprs: 0,
            num_expr_pats: 0,
            num_branches: 0,
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
                self.num_exprs += 1;
                self.num_tuple_item_exprs += tuple.len();
                for element in tuple.iter_elements() {
                    self.count_in_expr(element);
                }
            }
            ast::Expr::Closure(closure) => {
                self.num_exprs += 1;
                self.num_branches += closure.num_branches();

                for branch in closure.iter_branches() {
                    self.num_expr_pats += match branch.params {
                        ast::ExprParams::Zero => 0,
                        ast::ExprParams::One(..) => 1,
                        ast::ExprParams::Two(..) => 2,
                    };

                    self.count_in_expr(branch.body);
                }
            }
            ast::Expr::Appl(appl) => {
                self.num_exprs += 1;
                self.count_in_expr(appl.lhs);
                self.count_in_expr(appl.function);
                self.count_in_expr(appl.rhs);
            }
            ast::Expr::Symbol(_) => {}
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Integer(_) | ast::ExprLit::Ident(_) => self.num_exprs += 1,
                ast::ExprLit::True(_) | ast::ExprLit::False(_) => {}
            },
        }
    }
}

/// A base type that owns _most_ memory allocations in the HIR.
/// This type is intended to be borrowed from by [`Env::new`],
/// and the purpose behind creating a distinction between the two is that
/// we only ever want to allow for immutable references to the arenas but
/// mutable references for the vectors. By making `Env` borrow each allocation
/// from an `Hir` appropriately, we can freely mutate `Env` because it only
/// holds shared references to the arenas so no aliasing invariants are broken.
pub struct Hir<'hir, 'input> {
    types: Arena<Type<'hir>>,
    typevars: Vec<Typevar<'hir>>,
    exprs: Arena<Expr<'hir, 'input>>,
    expr_pats: Arena<ExprPat<'hir, 'input>>,
    expr_branches: Arena<ExprBranch<'hir, 'input>>,
    equations: Equations<'hir>,
}

impl<'hir, 'input> Hir<'hir, 'input> {
    pub fn new(counter: AllocationCounter) -> Self {
        Hir {
            types: Arena::with_capacity(1024), // need to precompute this...
            typevars: Vec::new(),
            exprs: Arena::with_capacity(counter.num_exprs),
            expr_pats: Arena::with_capacity(counter.num_expr_pats),
            expr_branches: Arena::with_capacity(counter.num_branches),
            equations: Equations::new(),
        }
    }
}

pub struct Env<'hir, 'input> {
    types: &'hir Arena<Type<'hir>>,
    exprs: &'hir Arena<Expr<'hir, 'input>>,
    expr_pats: &'hir Arena<ExprPat<'hir, 'input>>,
    tuple_item_exprs: &'hir Arena<List<'hir, &'hir Expr<'hir, 'input>>>,
    tuple_item_types: &'hir Arena<List<'hir, &'hir Type<'hir>>>,
    tuple_item_expr_pats: &'hir Arena<List<'hir, &'hir ExprPat<'hir, 'input>>>,
    expr_branches: &'hir Arena<List<'hir, ExprBranch<'hir, 'input>>>,
    typevars: &'hir mut Vec<Typevar<'hir>>,
    equations: &'hir mut Equations<'hir>,
}

impl<'hir, 'input> Env<'hir, 'input> {
    // pub fn new(hir: &'hir mut Hir<'hir, 'input>) -> Self {
    //     Env {
    //         types: &hir.types,
    //         typevars: &mut hir.typevars,
    //         exprs: &hir.exprs,
    //         expr_pats: &hir.expr_pats,
    //         expr_branches: &hir.expr_branches,
    //         equations: &mut hir.equations,
    //     }
    // }

    pub fn new_typevar(&mut self) -> (Var, &'hir Type<'hir>) {
        let var_ptr = Var(self.typevars.len());
        self.typevars.push(None);
        (var_ptr, self.types.push(Type::Var(var_ptr)))
    }

    /// Get a global environment with the type signatures of `in` and `print`
    /// already loaded in.
    ///
    /// `in`: `x (x () -> y) -> y`
    ///
    /// and
    ///
    /// `print`: `x () -> ()`
    pub fn default_globals(&mut self) -> impl Iterator<Item = (&'hir str, Polytype<'hir>)> {
        [
            ("in", {
                let (x, x_type) = self.new_typevar();
                let (y, y_type) = self.new_typevar();

                Polytype {
                    typevars: vec![x, y],
                    typ: self.types.push(Type::Function(TypeFunction {
                        lhs: x_type,
                        rhs: self.types.push(Type::Function(TypeFunction {
                            lhs: x_type,
                            rhs: &Type::Unit,
                            output: y_type,
                        })),
                        output: y_type,
                    })),
                }
            }),
            ("print", {
                let (x, x_type) = self.new_typevar();

                Polytype {
                    typevars: vec![x],
                    typ: self.types.push(Type::Function(TypeFunction {
                        lhs: x_type,
                        rhs: &Type::Unit,
                        output: &Type::Unit,
                    })),
                }
            }),
        ]
        .into_iter()
    }

    pub fn monomorphize(&mut self, polytype: &Polytype<'hir>) -> &'hir Type<'hir> {
        // Takes a polymorphic type and replaces all instances of generics
        // with a fixed, unbound type.
        // For example, id: T -> T is a polymorphic type, so it goes through
        // and replaces both `T`s with an unbound type variable like `a0`,
        // which is then bound later on.
        fn replace_unbound_typevars<'hir>(
            tbl: &HashMap<Var, &'hir Type<'hir>>,
            env: &mut Env<'hir, '_>,
            ty: &'hir Type<'hir>,
        ) -> &'hir Type<'hir> {
            match ty {
                Type::Var(var) => {
                    if let Some((t, _)) = env.typevars[var.0] {
                        replace_unbound_typevars(tbl, env, t)
                    } else if let Some(t) = tbl.get(var) {
                        t
                    } else {
                        ty
                    }
                }
                Type::Function(TypeFunction { lhs, rhs, output }) => {
                    env.types.push(Type::Function(TypeFunction {
                        lhs: replace_unbound_typevars(tbl, env, lhs),
                        rhs: replace_unbound_typevars(tbl, env, rhs),
                        output: replace_unbound_typevars(tbl, env, output),
                    }))
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

    /// Convert an [`ast::Type`] annotation into an HIR [`Type`].
    pub fn type_from_ast(
        &mut self,
        typ: &ast::Type<'_, 'input>,
        map: &HashMap<&str, &'hir Type<'hir>>,
    ) -> &'hir Type<'hir> {
        match typ {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => &Type::I32,
                "bool" => &Type::Bool,
                other => map.get(other).expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                // Build up the linked list of types from the inside out
                fn rec<'ast, 'hir, 'input: 'ast>(
                    env: &mut Env<'hir, 'input>,
                    map: &HashMap<&str, &'hir Type<'hir>>,
                    mut types: impl Iterator<Item = &'ast ast::Type<'ast, 'input>>,
                ) -> Option<&'hir List<'hir, &'hir Type<'hir>>> {
                    let item = env.type_from_ast(types.next()?, map);
                    Some(env.tuple_item_types.push(List {
                        item,
                        next: rec(env, map, types),
                    }))
                }

                if let Some(ty) = rec(self, map, tuple.iter_elements().copied()) {
                    self.types.push(Type::Tuple(ty))
                } else {
                    &Type::Unit
                }
            }
            ast::Type::Function(function) => self.types.push(Type::Function(TypeFunction {
                lhs: self.type_from_ast(function.lhs, map),
                rhs: self.type_from_ast(function.rhs, map),
                output: self.type_from_ast(function.ret, map),
            })),
        }
    }
}

#[derive(Debug)]
pub struct PushedErrors;

fn occurs<'hir>(typevars: &[Typevar<'hir>], var: Var, ty: &'hir Type<'hir>) -> bool {
    match ty {
        Type::Var(typevar) => {
            if let Some((t, _)) = typevars[typevar.0] {
                occurs(typevars, var, t)
            } else {
                var == *typevar
            }
        }
        Type::Function(TypeFunction { lhs, rhs, output }) => {
            occurs(typevars, var, lhs)
                || occurs(typevars, var, rhs)
                || occurs(typevars, var, output)
        }
        _ => false,
    }
}
