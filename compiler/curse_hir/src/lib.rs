#![allow(dead_code)]
use curse_ast as ast;
use displaydoc::Display;
use expr::*;
use petgraph::graph::NodeIndex;
use std::{collections::HashMap, fmt, num};
use thiserror::Error;
use typed_arena::Arena;

mod equations;
use equations::{Equations, Equiv, Reason, Rule};
mod expr;
mod hm;
mod hm2;
mod hm3;

#[cfg(test)]
mod tests;

/// `Some` is bound, `None` is unbound.
pub type Typevar<'hir> = Option<(&'hir Type<'hir>, NodeIndex)>;

/// Newtype around a `usize` used to index into the `typevars` field of `Env`.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("T{0}")]
pub struct Var(usize);

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'hir> {
    #[displaydoc("i32")]
    I32,
    #[displaydoc("bool")]
    Bool,
    #[displaydoc("{0}")]
    Tuple(TypeTuple<'hir>),
    #[displaydoc("{0}")]
    Var(Var),
    #[displaydoc("{0}")]
    Function(TypeFunction<'hir>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeTuple<'hir>(Vec<&'hir Type<'hir>>);

impl fmt::Display for TypeTuple<'_> {
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

#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("({lhs} {rhs} -> {output})")]
pub struct TypeFunction<'hir> {
    lhs: &'hir Type<'hir>,
    rhs: &'hir Type<'hir>,
    output: &'hir Type<'hir>,
}

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
    globals: &'hir HashMap<String, Polytype<'hir>>,
    locals: &'scope mut Vec<(String, &'hir Type<'hir>)>,
    original_len: usize,
}

impl<'scope, 'hir> Bindings<'scope, 'hir> {
    pub fn new(
        globals: &'hir HashMap<String, Polytype<'hir>>,
        locals: &'scope mut Vec<(String, &'hir Type<'hir>)>,
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

    pub fn add_local(&mut self, var: String, ty: &'hir Type<'hir>) {
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
pub enum TypeError<'hir> {
    #[error("Cannot unify types")]
    Unify(&'hir Type<'hir>, &'hir Type<'hir>),
    #[error("Cyclic type")]
    CyclicType(Var, &'hir Type<'hir>),
    #[error("Type not found")]
    NotFound,
    #[error(transparent)]
    ParseInt(num::ParseIntError),
}

static UNIT_TY: Type<'static> = Type::Tuple(TypeTuple(Vec::new()));

static UNIT_PAT: ExprPat<'static, 'static> = ExprPat::Tuple(ExprTuple {
    exprs: Vec::new(),
    ty: &UNIT_TY,
});

/// A base type that owns _most_ memory allocations in the HIR.
/// This type is intended to be borrowed from by [`Env::new`],
/// and the purpose behind creating a distinction between the two is that
/// we only ever want to allow for immutable references to the arenas but
/// mutable references for the vectors. By making `Env` borrow each allocation
/// from a `Allocations` appropriately, we can freely mutate `Env` because it only
/// holds shared references to the arenas so no aliasing invariants are broken.
#[derive(Default)]
pub struct Allocations<'hir, 'input> {
    types: Arena<Type<'hir>>,
    typevars: Vec<Typevar<'hir>>,
    exprs: Arena<Expr<'hir, 'input>>,
    expr_pats: Arena<ExprPat<'hir, 'input>>,
    expr_branches: Arena<ExprBranch<'hir, 'input>>,
    equations: Equations<'hir>,
}

pub struct Env<'hir, 'input> {
    types: &'hir Arena<Type<'hir>>,
    typevars: &'hir mut Vec<Typevar<'hir>>,
    exprs: &'hir Arena<Expr<'hir, 'input>>,
    expr_pats: &'hir Arena<ExprPat<'hir, 'input>>,
    expr_branches: &'hir Arena<ExprBranch<'hir, 'input>>,
    equations: &'hir mut Equations<'hir>,
}

impl<'hir, 'input> Env<'hir, 'input> {
    pub fn new(allocations: &'hir mut Allocations<'hir, 'input>) -> Self {
        Env {
            types: &allocations.types,
            typevars: &mut allocations.typevars,
            exprs: &allocations.exprs,
            expr_pats: &allocations.expr_pats,
            expr_branches: &allocations.expr_branches,
            equations: &mut allocations.equations,
        }
    }

    pub fn new_typevar(&mut self) -> (Var, &'hir Type<'hir>) {
        let var_ptr = Var(self.typevars.len());
        self.typevars.push(None);
        (var_ptr, self.types.alloc(Type::Var(var_ptr)))
    }

    /// Get a global environment with the type signatures of `in` and `print`
    /// already loaded in.
    ///
    /// `in`: `x (x () -> y) -> y`
    ///
    /// and
    ///
    /// `print`: `x () -> ()`
    pub fn default_globals(&mut self) -> impl Iterator<Item = (String, Polytype<'hir>)> {
        [
            ("in".to_string(), {
                let (x, x_type) = self.new_typevar();
                let (y, y_type) = self.new_typevar();

                Polytype {
                    typevars: vec![x, y],
                    typ: self.types.alloc(Type::Function(TypeFunction {
                        lhs: x_type,
                        rhs: self.types.alloc(Type::Function(TypeFunction {
                            lhs: x_type,
                            rhs: &UNIT_TY,
                            output: y_type,
                        })),
                        output: y_type,
                    })),
                }
            }),
            ("print".to_string(), {
                let (x, x_type) = self.new_typevar();

                Polytype {
                    typevars: vec![x],
                    typ: self.types.alloc(Type::Function(TypeFunction {
                        lhs: x_type,
                        rhs: &UNIT_TY,
                        output: &UNIT_TY,
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
                    env.types.alloc(Type::Function(TypeFunction {
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
        errors: &mut Vec<TypeError<'hir>>,
    ) -> Option<&'hir Expr<'hir, 'input>> {
        match expr {
            ast::Expr::Paren(parenthesized) => {
                self.lower(bindings, parenthesized.expr, map, errors)
            }
            ast::Expr::Symbol(symbol) => match symbol {
                ast::ExprSymbol::Plus(_) => Some(&Expr::Builtin(Builtin::Add)),
                ast::ExprSymbol::Minus(_) => Some(&Expr::Builtin(Builtin::Sub)),
                ast::ExprSymbol::Star(_) => Some(&Expr::Builtin(Builtin::Mul)),
                ast::ExprSymbol::Percent(_) => Some(&Expr::Builtin(Builtin::Rem)),
                ast::ExprSymbol::Slash(_) => Some(&Expr::Builtin(Builtin::Div)),
                ast::ExprSymbol::Dot(_) => todo!("infer ."),
                ast::ExprSymbol::DotDot(_) => todo!("infer .."),
                ast::ExprSymbol::Semi(_) => todo!("infer ;"),
                ast::ExprSymbol::Equal(_) => Some(&Expr::Builtin(Builtin::Eq)),
                ast::ExprSymbol::Less(_) => Some(&Expr::Builtin(Builtin::Lt)),
                ast::ExprSymbol::Greater(_) => Some(&Expr::Builtin(Builtin::Gt)),
                ast::ExprSymbol::LessEqual(_) => Some(&Expr::Builtin(Builtin::Le)),
                ast::ExprSymbol::GreaterEqual(_) => Some(&Expr::Builtin(Builtin::Ge)),
            },
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Some(self.exprs.alloc(Expr::I32(int))),
                    Err(e) => {
                        errors.push(TypeError::ParseInt(e));
                        None
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    if let Some(ty) = bindings.get(ident.literal, self) {
                        Some(self.exprs.alloc(Expr::Ident(ExprIdent {
                            literal: ident.literal,
                            ty,
                        })))
                    } else {
                        errors.push(TypeError::NotFound);
                        None
                    }
                }
                ast::ExprLit::True(_) => Some(&Expr::Bool(true)),
                ast::ExprLit::False(_) => Some(&Expr::Bool(false)),
            },
            ast::Expr::Tuple(tuple) => {
                let mut results = Some((
                    Vec::with_capacity(tuple.len()),
                    Vec::with_capacity(tuple.len()),
                ));

                for expr in tuple.iter_elements() {
                    match (&mut results, self.lower(bindings, expr, map, errors)) {
                        (Some((exprs, types)), Some(expr)) => {
                            exprs.push(expr);
                            types.push(expr.ty());
                        }
                        _ => results = None,
                    }
                }

                let (exprs, types) = results?;
                let ty = self.types.alloc(Type::Tuple(TypeTuple(types)));
                Some(self.exprs.alloc(Expr::Tuple(ExprTuple { exprs, ty })))
            }
            ast::Expr::Closure(ast::ExprClosure { head, tail }) => {
                let head = {
                    let mut scoped_bindings = bindings.enter_scope();

                    let body = self.lower(&mut scoped_bindings, head.body, map, errors)?;
                    let (lhs, rhs) =
                        self.type_of_many_params(&mut scoped_bindings, &head.params, map, errors)?;
                    ExprBranch { lhs, rhs, body }
                };

                let branches = tail
                    .iter()
                    .map(|(_, branch)| {
                        let mut scoped_bindings = bindings.enter_scope();

                        let body = self.lower(&mut scoped_bindings, branch.body, map, errors)?;
                        let (lhs, rhs) = self.type_of_many_params(
                            &mut scoped_bindings,
                            &branch.params,
                            map,
                            errors,
                        )?;

                        let typevars = self.typevars.as_mut_slice();

                        let original_error_count = errors.len();
                        unify(typevars, head.lhs.ty(), lhs.ty(), self.equations, errors);
                        unify(typevars, head.rhs.ty(), rhs.ty(), self.equations, errors);
                        unify(typevars, head.body.ty(), body.ty(), self.equations, errors);
                        if errors.len() == original_error_count {
                            Some(ExprBranch { lhs, rhs, body })
                        } else {
                            None
                        }
                    })
                    .collect::<Option<_>>()?;

                let ty = self.types.alloc(Type::Function(TypeFunction {
                    lhs: head.lhs.ty(),
                    rhs: head.rhs.ty(),
                    output: head.body.ty(),
                }));

                Some(self.exprs.alloc(Expr::Closure(ExprClosure {
                    ty,
                    head,
                    tail: branches,
                })))
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
                    self.types.alloc(Type::Function(TypeFunction {
                        lhs: lhs.ty(),
                        rhs: rhs.ty(),
                        output: ty,
                    })),
                    self.equations,
                    errors,
                );

                if errors.len() == original_error_count {
                    Some(self.exprs.alloc(Expr::Appl(ExprAppl {
                        ty,
                        lhs,
                        function,
                        rhs,
                    })))
                } else {
                    None
                }
            }
        }
    }

    /// Returns the [`Type`] of an [`ast::ExprPat`].
    fn lower_pat(
        &mut self,
        bindings: &mut Bindings<'_, 'hir>,
        pat: &ast::ExprPat<'_, 'input>,
        errors: &mut Vec<TypeError<'hir>>,
    ) -> Option<&'hir ExprPat<'hir, 'input>> {
        match pat {
            ast::Pat::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Some(self.expr_pats.alloc(ExprPat::I32(int))),
                    Err(e) => {
                        errors.push(TypeError::ParseInt(e));
                        None
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    let ty = self.new_typevar().1;
                    bindings.add_local(ident.literal.to_string(), ty);
                    Some(self.expr_pats.alloc(ExprPat::Ident(ExprIdent {
                        literal: ident.literal,
                        ty,
                    })))
                }
                ast::ExprLit::True(_) => Some(&ExprPat::Bool(true)),
                ast::ExprLit::False(_) => Some(&ExprPat::Bool(false)),
            },
            ast::Pat::Tuple(tuple) => {
                let mut results = Some((
                    Vec::with_capacity(tuple.len()),
                    Vec::with_capacity(tuple.len()),
                ));

                for pat in tuple.iter_elements() {
                    match (&mut results, self.lower_pat(bindings, pat, errors)) {
                        (Some((pats, types)), Some(pat)) => {
                            pats.push(pat);
                            types.push(pat.ty());
                        }
                        _ => results = None,
                    }
                }
                let (pats, types) = results?;

                // PLEASE RENAME SOME THINGS ;_;
                Some(self.expr_pats.alloc(ExprPat::Tuple(ExprTuple {
                    exprs: pats,
                    ty: self.types.alloc(Type::Tuple(TypeTuple(types))),
                })))
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
        errors: &mut Vec<TypeError<'hir>>,
    ) -> Option<&'hir ExprPat<'hir, 'input>> {
        let pat_type = self.lower_pat(bindings, param.pat, errors)?;
        if let Some((_, annotation)) = param.ty {
            let t2 = self.type_from_ast(annotation, map);
            let typevars = self.typevars.as_mut_slice();

            let original_error_count = errors.len();
            unify(typevars, pat_type.ty(), t2, self.equations, errors);
            if errors.len() != original_error_count {
                return None;
            }
        }

        Some(pat_type)
    }

    /// Returns the [`Type`]s of various [`ast::ExprParams`].
    fn type_of_many_params(
        &mut self,
        bindings: &mut Bindings<'_, 'hir>,
        params: &ast::ExprParams<'_, 'input>,
        map: &HashMap<&str, &'hir Type<'hir>>,
        errors: &mut Vec<TypeError<'hir>>,
    ) -> Option<(&'hir ExprPat<'hir, 'input>, &'hir ExprPat<'hir, 'input>)> {
        match params {
            ast::ExprParams::Zero => Some((&UNIT_PAT, &UNIT_PAT)),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(bindings, lhs, map, errors);
                Some((lhs_type?, &UNIT_PAT))
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(bindings, lhs, map, errors);
                let rhs_type = self.lower_param(bindings, rhs, map, errors);
                Some((lhs_type?, rhs_type?))
            }
        }
    }

    /// Convert an [`ast::Type`] annotation into an HIR [`Type`].
    pub fn type_from_ast(
        &self,
        typ: &ast::Type<'_, '_>,
        map: &HashMap<&str, &'hir Type<'hir>>,
    ) -> &'hir Type<'hir> {
        // this function needs to fundamentally change...
        match typ {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => &Type::I32,
                "bool" => &Type::Bool,
                other => map.get(other).expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                let mut elements = Vec::with_capacity(tuple.len());
                for ty in tuple.iter_elements() {
                    elements.push(self.type_from_ast(ty, map));
                }
                self.types.alloc(Type::Tuple(TypeTuple(elements)))
            }
            ast::Type::Function(function) => self.types.alloc(Type::Function(TypeFunction {
                lhs: self.type_from_ast(function.lhs, map),
                rhs: self.type_from_ast(function.rhs, map),
                output: self.type_from_ast(function.ret, map),
            })),
        }
    }
}

fn unify<'hir>(
    typevars: &mut [Typevar<'hir>],
    t1: &'hir Type<'hir>,
    t2: &'hir Type<'hir>,
    equations: &mut Equations<'hir>,
    errors: &mut Vec<TypeError<'hir>>,
) -> NodeIndex {
    match (t1, t2) {
        (Type::I32, Type::I32) => equations.add_rule(Rule::Equivalent(t1, t2, Equiv::Yes)),
        (Type::Bool, Type::Bool) => equations.add_rule(Rule::Equivalent(t1, t2, Equiv::Yes)),
        (Type::Tuple(TypeTuple(tuple_a)), Type::Tuple(TypeTuple(tuple_b))) => {
            if tuple_a.len() == tuple_b.len() {
                let conclusion = equations.add_rule(Rule::Equivalent(t1, t2, Equiv::Yes));
                for (i, (a, b)) in std::iter::zip(tuple_a, tuple_b).enumerate() {
                    // Always doing this `original_error_count` right after we unify...
                    let original_error_count = errors.len();
                    let proof = unify(typevars, a, b, equations, errors);
                    if errors.len() != original_error_count {
                        equations.graph[conclusion] = Rule::Equivalent(t1, t2, Equiv::No);
                    }
                    equations.add_proof(proof, conclusion, Reason::Tuple(i));
                }
                conclusion
            } else {
                // different length tuples
                errors.push(TypeError::Unify(t1, t2));
                equations.add_rule(Rule::Equivalent(t1, t2, Equiv::No))
            }
        }
        (Type::Var(var), a) | (a, Type::Var(var)) => {
            if let Some((b, _binding_source)) = typevars[var.0] {
                let original_error_count = errors.len();
                let proof = unify(typevars, a, b, equations, errors);

                let equiv = if errors.len() == original_error_count {
                    Equiv::Yes
                } else {
                    Equiv::No
                };
                let conclusion = equations.add_rule(Rule::Equivalent(t1, t2, equiv));
                // If we wanted, we could also add an edge with `_binding_source`,
                // which tells us exactly where the typevar was bound.
                equations.add_proof(proof, conclusion, Reason::Transitivity);
                conclusion
            } else if t1 == t2 {
                equations.add_rule(Rule::Equivalent(t1, t2, Equiv::Yes))
            } else if occurs(typevars, *var, a) {
                errors.push(TypeError::CyclicType(*var, a));
                equations.add_rule(Rule::Equivalent(t1, t2, Equiv::No))
            } else {
                // The actual binding code is here
                let conclusion = equations.add_rule(Rule::Binding {
                    var: *var,
                    definition: a,
                });

                typevars[var.0] = Some((a, conclusion));
                conclusion
            }
        }
        (Type::Function(f1), Type::Function(f2)) => {
            let conclusion = equations.add_rule(Rule::Equivalent(t1, t2, Equiv::Yes));
            let original_error_count = errors.len();
            let lhs_proof = unify(typevars, f1.lhs, f2.lhs, equations, errors);
            let rhs_proof = unify(typevars, f1.rhs, f2.rhs, equations, errors);
            let output_proof = unify(typevars, f1.output, f2.output, equations, errors);

            if errors.len() != original_error_count {
                equations.graph[conclusion] = Rule::Equivalent(t1, t2, Equiv::No);
            }

            equations.add_proof(lhs_proof, conclusion, Reason::FunctionLhs);
            equations.add_proof(rhs_proof, conclusion, Reason::FunctionRhs);
            equations.add_proof(output_proof, conclusion, Reason::FunctionOutput);

            conclusion
        }
        _ => {
            errors.push(TypeError::Unify(t1, t2));
            equations.add_rule(Rule::Equivalent(t1, t2, Equiv::No))
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
