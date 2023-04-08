#![allow(dead_code)]
use curse_arena::Arena;
use curse_ast as ast;
use displaydoc::Display;
use expr::*;
use petgraph::graph::NodeIndex;
use std::{collections::HashMap, fmt, num};
use thiserror::Error;

// TODO(quinn): what I really want is to be able to iterate over the arena.
// I think this means I want an arena that returns immutable references when
// you allocate something. That way, we can still have helpful lifetime annotations
// and also be allowed to iterate because you're allowed multiple immut refs.

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
    Tuple(&'hir Cons<'hir, &'hir Self>),
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

// TODO(quinn): in order to actually lower everything, we also need to be able to
// iterator through what's in the environment...

#[derive(Clone, Debug, Display, PartialEq)]
pub enum KnownType<'hir> {
    #[displaydoc("i32")]
    I32,
    #[displaydoc("bool")]
    Bool,
    #[displaydoc("{0}")]
    Tuple(TypeTuple<&'hir Self>),
    #[displaydoc("{0}")]
    Function(TypeFunction<&'hir Self>),
}

/// Node in a linked list
#[derive(Clone, Debug, PartialEq)]
pub struct Cons<'list, T> {
    // doesn't take a ref because for closure branches, we don't want to
    // have to put them in another arena
    item: T,
    next: Option<&'list Self>,
}

impl<'list, T> Cons<'list, T> {
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

#[derive(Clone, Debug, PartialEq)]
pub struct TypeTuple<Ty>(Vec<Ty>);

impl<Ty: fmt::Display> fmt::Display for TypeTuple<Ty> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        if let Some((x, xs)) = self.0.split_first() {
            write!(f, "{x}")?;
            for x in xs {
                write!(f, ", {x}")?;
            }
        }
        write!(f, ")")
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

pub struct Bindings<'scope, 'hir> {
    globals: &'hir HashMap<&'hir str, Polytype<'hir>>,
    locals: &'scope mut Vec<(&'hir str, &'hir Type<'hir>)>,
    original_len: usize,
}

impl<'scope, 'hir> Bindings<'scope, 'hir> {
    pub fn new(
        globals: &'hir HashMap<&'hir str, Polytype<'hir>>,
        locals: &'scope mut Vec<(&'hir str, &'hir Type<'hir>)>,
    ) -> Self {
        let original_len = locals.len();
        Bindings {
            globals,
            locals,
            original_len,
        }
    }

    /// Search through local variables first, then search through global variables.
    pub fn get(&self, var: &str, env: &mut Env<'hir, '_>) -> Option<&'hir Type<'hir>> {
        self.locals
            .iter()
            .rev()
            .find_map(|(ident, ty)| (*ident == var).then_some(*ty))
            .or_else(|| {
                self.globals
                    .get(var)
                    .map(|polytype| env.monomorphize(polytype))
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
    pub fn enter_scope(&mut self) -> Bindings<'_, 'hir> {
        Bindings::new(self.globals, self.locals)
    }
}

impl Drop for Bindings<'_, '_> {
    fn drop(&mut self) {
        self.locals.truncate(self.original_len);
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
            ast::Expr::Paren(parens) => self.count_in_expr(&parens.expr),
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
                self.count_in_expr(&appl.lhs);
                self.count_in_expr(&appl.function);
                self.count_in_expr(&appl.rhs);
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
    typevars: &'hir mut Vec<Typevar<'hir>>,
    exprs: &'hir Arena<Expr<'hir, 'input>>,
    /// Arena for linked lists of tuple item exprs (point to next tuple item)
    tuple_item_exprs: &'hir Arena<Cons<'hir, &'hir Expr<'hir, 'input>>>,
    /// Arena for linked lists of tuple item types (point to next tuple item)
    tuple_item_types: &'hir Arena<Cons<'hir, &'hir Type<'hir>>>,
    tuple_item_expr_pats: &'hir Arena<Cons<'hir, &'hir ExprPat<'hir, 'input>>>,
    expr_pats: &'hir Arena<ExprPat<'hir, 'input>>,
    expr_branches: &'hir Arena<Cons<'hir, ExprBranch<'hir, 'input>>>,
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
                    } else if let Some(t) = tbl.get(&var) {
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
                other => other,
            }
        }

        let tvs_to_replace = polytype
            .typevars
            .iter()
            .map(|tv| (*tv, self.new_typevar().1))
            .collect();

        replace_unbound_typevars(&tvs_to_replace, self, polytype.typ)
    }

    pub fn lower(
        &mut self,
        bindings: &mut Bindings<'_, 'hir>,
        expr: &ast::Expr<'_, 'input>,
        map: &HashMap<&str, &'hir Type<'hir>>,
        errors: &mut Vec<LowerError<'hir>>,
    ) -> Result<&'hir Expr<'hir, 'input>, PushedErrors> {
        match expr {
            ast::Expr::Paren(parenthesized) => {
                self.lower(bindings, parenthesized.expr, map, errors)
            }
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
                    Ok(int) => Ok(self.exprs.push(Expr::I32(int))),
                    Err(e) => {
                        errors.push(LowerError::ParseInt(e));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    if let Some(ty) = bindings.get(ident.literal, self) {
                        Ok(self.exprs.push(Expr::Ident(ExprIdent {
                            literal: ident.literal,
                            ty,
                        })))
                    } else {
                        errors.push(LowerError::IdentNotFound(ident.literal.to_string()));
                        Err(PushedErrors)
                    }
                }
                ast::ExprLit::True(_) => Ok(&Expr::Bool(true)),
                ast::ExprLit::False(_) => Ok(&Expr::Bool(false)),
            },
            ast::Expr::Tuple(tuple) => {
                // tuple rec fn
                fn rec<'ast, 'hir, 'input: 'ast>(
                    bindings: &mut Bindings<'_, 'hir>,
                    env: &mut Env<'hir, 'input>,
                    map: &HashMap<&str, &'hir Type<'hir>>,
                    errors: &mut Vec<LowerError<'hir>>,
                    mut exprs: impl Iterator<Item = &'ast ast::Expr<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir Cons<'hir, &'hir Expr<'hir, 'input>>,
                        &'hir Cons<'hir, &'hir Type<'hir>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(expr) = exprs.next() else {
                        return Ok(None);
                    };

                    let expr = env.lower(bindings, expr, map, errors)?;

                    let Some((next_expr, next_type)) = rec(bindings, env, map, errors, exprs)? else {
                        return Ok(None);
                    };

                    Ok(Some((
                        env.tuple_item_exprs.push(Cons {
                            item: expr,
                            next: Some(next_expr),
                        }),
                        env.tuple_item_types.push(Cons {
                            item: expr.ty(),
                            next: Some(next_type),
                        }),
                    )))
                }

                if let Some((exprs, ty)) =
                    rec(bindings, self, map, errors, tuple.iter_elements().copied())?
                {
                    Ok(self.exprs.push(Expr::Tuple(ExprTuple {
                        ty: self.types.push(Type::Tuple(ty)),
                        exprs,
                    })))
                } else {
                    Ok(&Expr::Unit)
                }
            }
            ast::Expr::Closure(ast::ExprClosure { head, tail }) => {
                let mut scoped_bindings = bindings.enter_scope();

                // Need to parse the params _before_ the body...
                // Duh.
                let (lhs, rhs) =
                    self.type_of_many_params(&mut scoped_bindings, &head.params, map, errors)?;
                let body = self.lower(&mut scoped_bindings, head.body, map, errors)?;

                drop(scoped_bindings);

                fn rec<'ast, 'hir, 'input: 'ast>(
                    head_lhs: &'hir Type<'hir>,
                    head_rhs: &'hir Type<'hir>,
                    head_body: &'hir Type<'hir>,
                    env: &mut Env<'hir, 'input>,
                    bindings: &mut Bindings<'_, 'hir>,
                    mut branches: impl Iterator<Item = &'ast ast::ExprBranch<'ast, 'input>>,
                    map: &HashMap<&str, &'hir Type<'hir>>,
                    errors: &mut Vec<LowerError<'hir>>,
                ) -> Result<Option<&'hir Cons<'hir, ExprBranch<'hir, 'input>>>, PushedErrors>
                {
                    let Some(branch) = branches.next() else {
                        return Ok(None);
                    };

                    let mut scoped_bindings = bindings.enter_scope();

                    let (lhs, rhs) =
                        env.type_of_many_params(&mut scoped_bindings, &branch.params, map, errors)?;
                    let body = env.lower(&mut scoped_bindings, branch.body, map, errors)?;
                    drop(scoped_bindings);

                    let typevars = env.typevars.as_mut_slice();

                    let original_error_count = errors.len();
                    unify(typevars, head_lhs, lhs.ty(), env.equations, errors);
                    unify(typevars, head_rhs, rhs.ty(), env.equations, errors);
                    unify(typevars, head_body, body.ty(), env.equations, errors);

                    if errors.len() == original_error_count {
                        Ok(Some(env.expr_branches.push(Cons {
                            item: ExprBranch { lhs, rhs, body },
                            next: rec(
                                head_lhs, head_rhs, head_body, env, bindings, branches, map, errors,
                            )?,
                        })))
                    } else {
                        Err(PushedErrors)
                    }
                }

                let next = rec(
                    lhs.ty(),
                    rhs.ty(),
                    body.ty(),
                    self,
                    bindings,
                    tail.iter().map(|(_, branch)| branch),
                    map,
                    errors,
                )?;

                let head = ExprBranch { lhs, rhs, body };

                let branches = self.expr_branches.push(Cons { item: head, next });

                let ty = self.types.push(Type::Function(TypeFunction {
                    lhs: head.lhs.ty(),
                    rhs: head.rhs.ty(),
                    output: head.body.ty(),
                }));

                Ok(self.exprs.push(Expr::Closure(ExprClosure { ty, branches })))
            }
            ast::Expr::Appl(appl) => {
                let lhs = self.lower(bindings, appl.lhs, map, errors);
                let rhs = self.lower(bindings, appl.rhs, map, errors);
                let function = self.lower(bindings, appl.function, map, errors);

                let (lhs, rhs, function) = (lhs?, rhs?, function?);

                let ty = self.new_typevar().1;

                let typevars = self.typevars.as_mut_slice();
                let original_error_count = errors.len();
                unify(
                    typevars,
                    function.ty(),
                    self.types.push(Type::Function(TypeFunction {
                        lhs: lhs.ty(),
                        rhs: rhs.ty(),
                        output: ty,
                    })),
                    self.equations,
                    errors,
                );

                if errors.len() == original_error_count {
                    Ok(self.exprs.push(Expr::Appl(ExprAppl {
                        ty,
                        lhs,
                        function,
                        rhs,
                    })))
                } else {
                    Err(PushedErrors)
                }
            }
        }
    }

    /// Returns the [`Type`] of an [`ast::ExprPat`].
    fn lower_pat(
        &mut self,
        bindings: &mut Bindings<'_, 'hir>,
        pat: &ast::ExprPat<'_, 'input>,
        errors: &mut Vec<LowerError<'hir>>,
    ) -> Result<&'hir ExprPat<'hir, 'input>, PushedErrors> {
        match pat {
            ast::Pat::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(self.expr_pats.push(ExprPat::I32(int))),
                    Err(e) => {
                        errors.push(LowerError::ParseInt(e));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    let ty = self.new_typevar().1;
                    bindings.add_local(ident.literal, ty);
                    Ok(self.expr_pats.push(ExprPat::Ident(ExprIdent {
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
                    bindings: &mut Bindings<'_, 'hir>,
                    env: &mut Env<'hir, 'input>,
                    errors: &mut Vec<LowerError<'hir>>,
                    mut pats: impl Iterator<Item = &'ast ast::ExprPat<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir Cons<'hir, &'hir ExprPat<'hir, 'input>>,
                        &'hir Cons<'hir, &'hir Type<'hir>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(pat) = pats.next() else {
                        return Ok(None);
                    };

                    let pat = env.lower_pat(bindings, pat, errors)?;

                    let Some((next_pat, next_type)) = rec(bindings, env, errors, pats)? else {
                        return Ok(None);
                    };

                    Ok(Some((
                        env.tuple_item_expr_pats.push(Cons {
                            item: pat,
                            next: Some(next_pat),
                        }),
                        env.tuple_item_types.push(Cons {
                            item: pat.ty(),
                            next: Some(next_type),
                        }),
                    )))
                }

                if let Some((exprs, ty)) =
                    rec(bindings, self, errors, tuple.iter_elements().copied())?
                {
                    Ok(self.expr_pats.push(ExprPat::Tuple(ExprTuple {
                        ty: self.types.push(Type::Tuple(ty)),
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
        bindings: &mut Bindings<'_, 'hir>,
        param: &ast::ExprParam<'_, 'input>,
        map: &HashMap<&str, &'hir Type<'hir>>,
        errors: &mut Vec<LowerError<'hir>>,
    ) -> Result<&'hir ExprPat<'hir, 'input>, PushedErrors> {
        let pat_type = self.lower_pat(bindings, param.pat, errors)?;
        if let Some((_, annotation)) = param.ty {
            let t2 = self.type_from_ast(annotation, map);
            let typevars = self.typevars.as_mut_slice();

            let original_error_count = errors.len();
            unify(typevars, pat_type.ty(), t2, self.equations, errors);
            if errors.len() != original_error_count {
                return Err(PushedErrors);
            }
        }

        Ok(pat_type)
    }

    /// Returns the [`Type`]s of various [`ast::ExprParams`].
    fn type_of_many_params(
        &mut self,
        bindings: &mut Bindings<'_, 'hir>,
        params: &ast::ExprParams<'_, 'input>,
        map: &HashMap<&str, &'hir Type<'hir>>,
        errors: &mut Vec<LowerError<'hir>>,
    ) -> Result<(&'hir ExprPat<'hir, 'input>, &'hir ExprPat<'hir, 'input>), PushedErrors> {
        match params {
            ast::ExprParams::Zero => Ok((&ExprPat::Unit, &ExprPat::Unit)),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(bindings, lhs, map, errors);
                Ok((lhs_type?, &ExprPat::Unit))
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(bindings, lhs, map, errors);
                let rhs_type = self.lower_param(bindings, rhs, map, errors);
                Ok((lhs_type?, rhs_type?))
            }
        }
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
                ) -> Option<&'hir Cons<'hir, &'hir Type<'hir>>> {
                    let item = env.type_from_ast(types.next()?, map);
                    Some(env.tuple_item_types.push(Cons {
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

fn unify<'hir>(
    typevars: &mut [Typevar<'hir>],
    t1: &'hir Type<'hir>,
    t2: &'hir Type<'hir>,
    equations: &mut Equations<'hir>,
    errors: &mut Vec<LowerError<'hir>>,
) -> NodeIndex {
    match (t1, t2) {
        (Type::I32, Type::I32) => equations.add_rule(Node::Equiv(t1, t2)),
        (Type::Bool, Type::Bool) => equations.add_rule(Node::Equiv(t1, t2)),
        (Type::Unit, Type::Unit) => equations.add_rule(Node::Equiv(t1, t2)),
        (Type::Tuple(a), Type::Tuple(b)) => {
            if a.len() == b.len() {
                let conclusion = equations.add_rule(Node::Equiv(t1, t2));
                for (i, (a, b)) in a.iter().zip(b.iter()).enumerate() {
                    // Always doing this `original_error_count` right after we unify...
                    let original_error_count = errors.len();
                    let proof = unify(typevars, a, b, equations, errors);
                    if errors.len() != original_error_count {
                        equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                    }
                    equations.add_proof(proof, conclusion, Edge::Tuple(i));
                }
                conclusion
            } else {
                // different length tuples
                errors.push(LowerError::Unify(t1, t2));
                equations.add_rule(Node::NotEquiv(t1, t2))
            }
        }
        (Type::Var(var), a) | (a, Type::Var(var)) => {
            if let Some((b, _binding_source)) = typevars[var.0] {
                let original_error_count = errors.len();
                let proof = unify(typevars, a, b, equations, errors);

                let conclusion = if errors.len() == original_error_count {
                    equations.add_rule(Node::Equiv(t1, t2))
                } else {
                    equations.add_rule(Node::NotEquiv(t1, t2))
                };
                // If we wanted, we could also add an edge with `_binding_source`,
                // which tells us exactly where the typevar was bound.
                equations.add_proof(proof, conclusion, Edge::Transitivity);
                conclusion
            } else if t1 == t2 {
                equations.add_rule(Node::Equiv(t1, t2))
            } else if occurs(typevars, *var, a) {
                errors.push(LowerError::CyclicType(*var, a));
                equations.add_rule(Node::NotEquiv(t1, t2))
            } else {
                // The actual binding code is here
                let conclusion = equations.add_rule(Node::Binding {
                    var: *var,
                    definition: a,
                });

                typevars[var.0] = Some((a, conclusion));
                conclusion
            }
        }
        (Type::Function(f1), Type::Function(f2)) => {
            let conclusion = equations.add_rule(Node::Equiv(t1, t2));
            let original_error_count = errors.len();
            let lhs_proof = unify(typevars, f1.lhs, f2.lhs, equations, errors);
            let rhs_proof = unify(typevars, f1.rhs, f2.rhs, equations, errors);
            let output_proof = unify(typevars, f1.output, f2.output, equations, errors);

            if errors.len() != original_error_count {
                equations.graph[conclusion] = Node::NotEquiv(t1, t2);
            }

            equations.add_proof(lhs_proof, conclusion, Edge::FunctionLhs);
            equations.add_proof(rhs_proof, conclusion, Edge::FunctionRhs);
            equations.add_proof(output_proof, conclusion, Edge::FunctionOutput);

            conclusion
        }
        _ => {
            errors.push(LowerError::Unify(t1, t2));
            equations.add_rule(Node::NotEquiv(t1, t2))
        }
    }
}

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
