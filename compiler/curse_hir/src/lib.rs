#![allow(dead_code)]
use curse_ast as ast;
use displaydoc::Display;
use expr::*;
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt, num,
};
use thiserror::Error;
use typed_arena::Arena;

mod expr;
mod hm;
mod hm2;
mod hm3;

#[cfg(test)]
mod tests;

/// Newtype around a `u64` representing a unique, fixed type that is not yet fixed.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("a{0}")]
pub struct UnboundVar(u64);

/// Newtype around a `usize` used to index into the `typevars` field of `Env`.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("a{0}")]
pub struct VarPtr(usize);

/// Newtype around a `usize`
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictPtr(usize);

#[derive(Clone, Debug, Display, PartialEq)]
pub enum Type<'hir> {
    // Type representing integer literals that don't explicitly
    // have a size. For example, `5` could be `i32` or `i64`,
    // and can be unified to either if necessary.
    // Thus, when unifying `UnknownInteger` and `i32`, they
    // will both come out as `i32`.
    #[displaydoc("{{integer}}")]
    UnknownInteger,
    #[displaydoc("i32")]
    I32,
    #[displaydoc("bool")]
    Bool,
    #[displaydoc("{0}")]
    Tuple(TypeTuple<'hir>),
    #[displaydoc("{0}")]
    Var(VarPtr),
    #[displaydoc("{0}")]
    Function(TypeFunction<'hir>),
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq, Display)]
enum Typevar<'hir> {
    #[displaydoc("{0}")]
    Bound(&'hir Type<'hir>),
    #[displaydoc("{0}")]
    Unbound(UnboundVar),
}

#[derive(Copy, Clone, Debug, Display, PartialEq)]
#[displaydoc("{lhs}, {rhs} -> {output}")]
pub struct TypeFunction<'hir> {
    lhs: &'hir Type<'hir>,
    rhs: &'hir Type<'hir>,
    output: &'hir Type<'hir>,
}

pub struct Polytype<'hir> {
    typevars: Vec<UnboundVar>,
    typ: &'hir Type<'hir>,
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
    pub fn get(&self, var: &str, env: &'hir Env<'hir, '_>) -> Option<&'hir Type<'hir>> {
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
    #[error("Cannot unify types `{0}` and `{1}`")]
    Unify(&'hir Type<'hir>, &'hir Type<'hir>),
    #[error("Cannot unify types in tuple")]
    UnifyTuple(Vec<TypeError<'hir>>),
    #[error("Cyclic type: attempted to bind type {0} to {1}")]
    CyclicType(UnboundVar, &'hir Type<'hir>),
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

#[derive(Default)]
pub struct Env<'hir, 'input> {
    next_id: Cell<u64>,
    types: Arena<Type<'hir>>,
    typevars: RefCell<Vec<Typevar<'hir>>>,
    exprs: Arena<Expr<'hir, 'input>>,
    expr_pats: Arena<ExprPat<'hir, 'input>>,
    expr_branches: Arena<ExprBranch<'hir, 'input>>,
}

impl<'hir, 'input> Env<'hir, 'input> {
    pub fn new() -> Self {
        Env {
            next_id: Cell::new(0),
            types: Arena::new(),
            typevars: RefCell::new(Vec::new()),
            exprs: Arena::new(),
            expr_pats: Arena::new(),
            expr_branches: Arena::new(),
        }
    }

    pub fn new_typevar(&'hir self) -> (UnboundVar, &'hir Type<'hir>) {
        let var = self.next_id.get();
        self.next_id.set(var + 1);
        let unbound_var = UnboundVar(var);

        let mut typevars = self.typevars.borrow_mut();

        let var_ptr = VarPtr(typevars.len());
        typevars.push(Typevar::Unbound(unbound_var));
        let typ = Type::Var(var_ptr);
        (unbound_var, self.types.alloc(typ))
    }

    /// Get a global environment with the type signatures of `in` and `print`
    /// already loaded in.
    ///
    /// `in`: `x (x () -> y) -> y`
    ///
    /// and
    ///
    /// `print`: `x () -> ()`
    pub fn default_globals(&'hir self) -> HashMap<String, Polytype<'hir>> {
        HashMap::from([
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
        ])
    }

    pub fn monomorphize(&'hir self, polytype: &Polytype<'hir>) -> &'hir Type<'hir> {
        // Takes a polymorphic type and replaces all instances of generics
        // with a fixed, unbound type.
        // For example, id: T -> T is a polymorphic type, so it goes through
        // and replaces both `T`s with an unbound type variable like `a0`,
        // which is then bound later on.
        fn replace_unbound_typevars<'hir>(
            tbl: &HashMap<UnboundVar, &'hir Type<'hir>>,
            env: &'hir Env<'hir, '_>,
            ty: &'hir Type<'hir>,
        ) -> &'hir Type<'hir> {
            match ty {
                Type::Var(var_ptr) => match env.typevars.borrow()[var_ptr.0] {
                    Typevar::Bound(t) => replace_unbound_typevars(tbl, env, t),
                    Typevar::Unbound(typevar_id) => match tbl.get(&typevar_id) {
                        Some(t) => t,
                        None => ty,
                    },
                },
                Type::Function(TypeFunction { lhs, rhs, output }) => {
                    env.types.alloc(Type::Function(TypeFunction {
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

    pub fn lower(
        &'hir self,
        bindings: &mut Bindings<'_, 'hir>,
        expr: &ast::Expr<'_, 'input>,
        errors: &mut Vec<TypeError<'hir>>,
    ) -> Option<&'hir Expr<'hir, 'input>> {
        match expr {
            ast::Expr::Paren(parenthesized) => self.lower(bindings, parenthesized.expr, errors),
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
                let mut result = Some((
                    Vec::with_capacity(tuple.len()),
                    Vec::with_capacity(tuple.len()),
                ));

                for expr in tuple.iter_elements() {
                    match (&mut result, self.lower(bindings, expr, errors)) {
                        (Some((exprs, types)), Some(expr)) => {
                            exprs.push(expr);
                            types.push(expr.ty());
                        }
                        _ => result = None,
                    }
                }

                result.map(|(expr, ty)| {
                    let ty = self.types.alloc(Type::Tuple(TypeTuple(ty)));
                    &*self.exprs.alloc(Expr::Tuple(ExprTuple { exprs: expr, ty }))
                })
            }
            ast::Expr::Closure(ast::ExprClosure { head, tail }) => {
                let head = {
                    let mut scoped_bindings = bindings.enter_scope();

                    // There's a lot to be done here in terms of not exiting immediately once
                    // an error occurs
                    let (lhs, rhs) =
                        self.type_of_many_params(&mut scoped_bindings, &head.params, errors)?;
                    let body = self.lower(&mut scoped_bindings, head.body, errors)?;

                    ExprBranch { lhs, rhs, body }
                };

                let mut branches = Some(Vec::with_capacity(tail.len()));

                let mut typevars = self.typevars.borrow_mut();
                let typevars = typevars.as_mut_slice();

                for (_, branch) in tail {
                    let mut scoped_bindings = bindings.enter_scope();
                    let Some((lhs, rhs)) = self.type_of_many_params(&mut scoped_bindings, &branch.params, errors) else {
                        continue;
                    };
                    let Some(body) = self.lower(&mut scoped_bindings, branch.body, errors) else {
                        continue;
                    };

                    if let Err(e) = unify(typevars, head.lhs.ty(), lhs.ty()) {
                        errors.push(e);
                        branches = None;
                    }
                    if let Err(e) = unify(typevars, head.rhs.ty(), rhs.ty()) {
                        errors.push(e);
                        branches = None;
                    }
                    if let Err(e) = unify(typevars, head.body.ty(), body.ty()) {
                        errors.push(e);
                        branches = None;
                    }

                    if let Some(branches) = &mut branches {
                        branches.push(ExprBranch { lhs, rhs, body })
                    }
                }

                let ty = self.types.alloc(Type::Function(TypeFunction {
                    lhs: head.lhs.ty(),
                    rhs: head.rhs.ty(),
                    output: head.body.ty(),
                }));

                branches.map(|tail| {
                    &*self
                        .exprs
                        .alloc(Expr::Closure(ExprClosure { ty, head, tail }))
                })
            }
            ast::Expr::Appl(appl) => {
                let lhs = self.lower(bindings, appl.lhs, errors)?;
                let rhs = self.lower(bindings, appl.rhs, errors)?;
                let function = self.lower(bindings, appl.function, errors)?;
                let ty = self.new_typevar().1;

                let mut typevars = self.typevars.borrow_mut();
                let typevars = typevars.as_mut_slice();
                match unify(
                    typevars,
                    function.ty(),
                    self.types.alloc(Type::Function(TypeFunction {
                        lhs: lhs.ty(),
                        rhs: rhs.ty(),
                        output: ty,
                    })),
                ) {
                    Ok(()) => Some(self.exprs.alloc(Expr::Appl(ExprAppl {
                        ty,
                        lhs,
                        function,
                        rhs,
                    }))),
                    Err(e) => {
                        errors.push(e);
                        None
                    }
                }
            }
        }
    }

    /// Returns the [`Type`] of an [`ast::ExprPat`].
    fn lower_pat(
        &'hir self,
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
                let mut result = Some((
                    Vec::with_capacity(tuple.len()),
                    Vec::with_capacity(tuple.len()),
                ));

                for pat in tuple.iter_elements() {
                    match (&mut result, self.lower_pat(bindings, pat, errors)) {
                        (Some((pats, types)), Some(pat)) => {
                            pats.push(pat);
                            types.push(pat.ty());
                        }
                        _ => result = None,
                    }
                }

                result.map(|(pats, types)| {
                    // PLEASE RENAME SOME THINGS ;_;
                    &*self.expr_pats.alloc(ExprPat::Tuple(ExprTuple {
                        exprs: pats,
                        ty: self.types.alloc(Type::Tuple(TypeTuple(types))),
                    }))
                })
            } // When we add struct destructuring, we can unify the type of the field
              // with the returned type of the pattern in that field.
              // e.g. If we have a `struct Number(i32)` and have the pattern
              // `Number(x)`, then `x` might be type `Var(Typevar::Unbound(0))` which
              // we can unify with `i32` to see that it should be `Var(Typevar::Bound(&Int))`
        }
    }

    /// Returns the [`Type`] of a single [`ast::ExprParam`].
    fn lower_param(
        &'hir self,
        bindings: &mut Bindings<'_, 'hir>,
        param: &ast::ExprParam<'_, 'input>,
        errors: &mut Vec<TypeError<'hir>>,
    ) -> Option<&'hir ExprPat<'hir, 'input>> {
        let pat_type = self.lower_pat(bindings, param.pat, errors)?; // dont short circuit here
        if let Some((_, annotation)) = param.ty {
            let mut typevars = self.typevars.borrow_mut();
            let typevars = typevars.as_mut_slice();
            if let Err(e) = unify(typevars, pat_type.ty(), self.type_from_ast(annotation)) {
                errors.push(e);
                return None;
            }
        }

        Some(pat_type)
    }

    /// Returns the [`Type`]s of various [`ast::ExprParams`].
    fn type_of_many_params(
        &'hir self,
        bindings: &mut Bindings<'_, 'hir>,
        params: &ast::ExprParams<'_, 'input>,
        errors: &mut Vec<TypeError<'hir>>,
    ) -> Option<(&'hir ExprPat<'hir, 'input>, &'hir ExprPat<'hir, 'input>)> {
        match params {
            ast::ExprParams::Zero => Some((&UNIT_PAT, &UNIT_PAT)),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(bindings, lhs, errors);
                Some((lhs_type?, &UNIT_PAT))
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(bindings, lhs, errors);
                let rhs_type = self.lower_param(bindings, rhs, errors);
                Some((lhs_type?, rhs_type?))
            }
        }
    }

    /// Convert an [`ast::Type`] annotation into an HIR [`Type`].
    pub fn type_from_ast(&'hir self, typ: &ast::Type<'_, '_>) -> &'hir Type<'hir> {
        match typ {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => &Type::I32,
                "bool" => &Type::Bool,
                other => todo!("name resolution (type is {other})"),
            },
            ast::Type::Tuple(tuple) => {
                let mut elements = Vec::with_capacity(tuple.len());
                for typ in tuple.iter_elements() {
                    elements.push(self.type_from_ast(typ));
                }
                self.types.alloc(Type::Tuple(TypeTuple(elements)))
            }
            ast::Type::Function(function) => self.types.alloc(Type::Function(TypeFunction {
                lhs: self.type_from_ast(function.lhs),
                rhs: self.type_from_ast(function.rhs),
                output: self.type_from_ast(function.ret),
            })),
        }
    }
}

// TODO: better error messages. For example, it would be really nice to have
// a way to interactively look at all the types within the source code and see
// where each inference comes from.
fn unify<'hir>(
    typevars: &mut [Typevar<'hir>],
    t1: &'hir Type<'hir>,
    t2: &'hir Type<'hir>,
) -> Result<(), TypeError<'hir>> {
    match (t1, t2) {
        (Type::I32, Type::I32) => Ok(()),
        (Type::Bool, Type::Bool) => Ok(()),
        (Type::Tuple(TypeTuple(tuple_a)), Type::Tuple(TypeTuple(tuple_b))) => {
            if tuple_a.len() == tuple_b.len() {
                let mut errors = Vec::with_capacity(0);
                for (a, b) in std::iter::zip(tuple_a, tuple_b) {
                    if let Err(e) = unify(typevars, a, b) {
                        errors.push(e);
                    }
                }
                if errors.is_empty() {
                    Ok(())
                } else {
                    Err(TypeError::UnifyTuple(errors))
                }
            } else {
                // different length tuples
                Err(TypeError::Unify(t1, t2))
            }
        }
        (Type::Var(typevar), a) | (a, Type::Var(typevar)) => match typevars[typevar.0] {
            Typevar::Bound(b) => unify(typevars, a, b),
            Typevar::Unbound(a_id) => {
                if t1 == t2 {
                    Ok(())
                } else if occurs(typevars, a_id, a) {
                    Err(TypeError::CyclicType(a_id, a))
                } else {
                    typevars[typevar.0] = Typevar::Bound(a);
                    Ok(())
                }
            }
        },
        (Type::Function(f1), Type::Function(f2)) => {
            unify(typevars, f1.lhs, f2.lhs)?;
            unify(typevars, f1.rhs, f2.rhs)?;
            unify(typevars, f1.output, f2.output)
        }
        _ => Err(TypeError::Unify(t1, t2)),
    }
}

fn occurs<'hir>(
    typevars: &[Typevar<'hir>],
    unbound_var: UnboundVar,
    typ: &'hir Type<'hir>,
) -> bool {
    match typ {
        Type::Var(typevar) => match typevars[typevar.0] {
            Typevar::Bound(t) => occurs(typevars, unbound_var, t),
            Typevar::Unbound(id2) => unbound_var == id2,
        },
        Type::Function(TypeFunction { lhs, rhs, output }) => [lhs, rhs, output]
            .into_iter()
            .any(|typ| occurs(typevars, unbound_var, typ)),
        _ => false,
    }
}
