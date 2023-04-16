#![allow(dead_code)]
use ast::Span;
use curse_ast as ast;
use displaydoc::Display;
use petgraph::graph::NodeIndex;
use smallvec::{smallvec, SmallVec};
use std::{
    collections::HashMap,
    fmt,
    ops::{Index, IndexMut},
};
use typed_arena::Arena;

mod equations;
pub use equations::{Edge, Equations, Node};
mod expr;
pub use expr::*;
pub mod dot;
mod error;
pub use error::*;
mod spanned;
pub use spanned::{WithSpan, S};

/// `Some` is bound, `None` is unbound.
pub type Typevar<'hir> = Option<(S<Type<'hir>>, NodeIndex)>;

#[derive(Copy, Clone, Display, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("T{0}")]
pub struct Var(usize);

#[derive(Copy, Clone)]
pub enum Type<'hir> {
    I32,
    Bool,
    Unit,
    Var(Var),
    Tuple(&'hir List<'hir, S<Type<'hir>>>),
    Function(&'hir TypeFunction<'hir>),
}

impl<'hir> Type<'hir> {
    /// Returns a [`Display`](fmt::Display)able type that prints a [`Type`],
    /// except with all type variables fully expanded as much as possible.
    pub fn pretty(self, hir: &'hir Hir<'hir, 'hir>) -> TypePrinter<'hir> {
        TypePrinter { ty: self, hir }
    }
}

impl fmt::Debug for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I32 => write!(f, "I32"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "Unit"),
            Self::Tuple(tuple) => f.debug_tuple("Tuple").field(tuple).finish(),
            Self::Var(var) => fmt::Debug::fmt(var, f),
            Self::Function(fun) => fmt::Debug::fmt(fun, f),
        }
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Tuple(tuple) => write!(f, "({})", tuple.delim(", ")),
            Type::Var(var) => write!(f, "{var}"),
            Type::Function(fun) => write!(f, "{fun}"),
        }
    }
}

pub struct TypePrinter<'a> {
    ty: Type<'a>,
    hir: &'a Hir<'a, 'a>,
}

impl fmt::Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Tuple(tuple) => {
                write!(f, "(")?;
                let mut iter = tuple.iter();
                if let Some(ty) = iter.next() {
                    write!(f, "{}", ty.value().pretty(self.hir))?;
                }
                for ty in iter {
                    write!(f, ", {}", ty.value().pretty(self.hir))?;
                }
                write!(f, ")")
            }
            Type::Var(var) => {
                if let Some((ty, _)) = &self.hir[var] {
                    write!(f, "{}", ty.value().pretty(self.hir))
                } else {
                    write!(f, "{var}")
                }
            }
            Type::Function(fun) => {
                write!(
                    f,
                    "({} {} -> {})",
                    fun.lhs.value().pretty(self.hir),
                    fun.rhs.value().pretty(self.hir),
                    fun.output.value().pretty(self.hir)
                )
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("({lhs} {rhs} -> {output})")]
pub struct TypeFunction<'hir> {
    lhs: S<Type<'hir>>,
    rhs: S<Type<'hir>>,
    output: S<Type<'hir>>,
}

#[derive(Copy, Clone, PartialEq)]
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
    /// Returns a [`Display`](fmt::Display)able type with a provided delimiter.
    pub fn delim<'a>(&'a self, delim: &'a str) -> Delim<'a, T> {
        Delim {
            list: self,
            sep: delim,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for List<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

pub struct Delim<'a, T> {
    list: &'a List<'a, T>,
    sep: &'a str,
}

impl<T: fmt::Display> fmt::Display for Delim<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.list.item.fmt(f)?;
        if let Some(remaining) = self.list.next {
            for item in remaining.iter() {
                write!(f, "{}{}", self.sep, item)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Polytype<'hir> {
    pub typevars: SmallVec<[Var; 4]>,
    pub ty: S<Type<'hir>>,
}

impl<'hir> Polytype<'hir> {
    pub fn new(ty: S<Type<'hir>>) -> Self {
        Polytype {
            typevars: SmallVec::new(),
            ty,
        }
    }
}

pub struct Scope<'outer, 'hir, 'input> {
    pub hir: &'outer mut Hir<'hir, 'input>,
    type_map: &'outer HashMap<&'outer str, S<Type<'hir>>>,
    errors: &'outer mut Vec<LowerError<'hir>>,
    original_errors_len: usize,
    globals: &'hir HashMap<&'hir str, Polytype<'hir>>,
    locals: &'outer mut Vec<(&'hir str, S<Type<'hir>>)>,
    original_locals_len: usize,
}

impl<'outer, 'hir, 'input: 'hir> Scope<'outer, 'hir, 'input> {
    pub fn new(
        hir: &'outer mut Hir<'hir, 'input>,
        type_map: &'outer HashMap<&'outer str, S<Type<'hir>>>,
        errors: &'outer mut Vec<LowerError<'hir>>,
        globals: &'hir HashMap<&'hir str, Polytype>,
        locals: &'outer mut Vec<(&'hir str, S<Type<'hir>>)>,
    ) -> Self {
        let original_errors_len = errors.len();
        let original_locals_len = locals.len();
        Scope {
            hir,
            type_map,
            errors,
            original_errors_len,
            globals,
            locals,
            original_locals_len,
        }
    }

    /// Search through local variables first, then search through global variables.
    pub fn type_of(&mut self, var: &str) -> Option<S<Type<'hir>>> {
        self.locals
            .iter()
            .rev()
            .find_map(|(ident, ty)| (*ident == var).then_some(*ty))
            .or_else(|| {
                self.globals
                    .get(var)
                    .map(|polytype| self.hir.monomorphize(polytype))
            })
    }

    pub fn add_local(&mut self, var: &'hir str, ty: S<Type<'hir>>) {
        self.locals.push((var, ty));
    }

    /// Enter a new scope.
    ///
    /// This method will uniquely borrow a `Scope` to create another `Scope`,
    /// which represents an inner scope. When the returned type is dropped,
    /// all bindings that were added in the inner scope will be removed,
    /// leaving the original scope in its initial state and accessible again
    /// since it's no longer borrowed.
    pub fn enter_scope(&mut self) -> Scope<'_, 'hir, 'input> {
        Scope::new(
            self.hir,
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
    ) -> Result<S<Expr<'hir, 'input>>, PushedErrors> {
        match expr {
            ast::Expr::Paren(paren) => self.lower(paren.expr),
            ast::Expr::Symbol(symbol) => match symbol {
                ast::ExprSymbol::Plus(plus) => {
                    Ok(Expr::Builtin(Builtin::Add).with_span(plus.span()))
                }
                ast::ExprSymbol::Minus(minus) => {
                    Ok(Expr::Builtin(Builtin::Sub).with_span(minus.span()))
                }
                ast::ExprSymbol::Star(star) => {
                    Ok(Expr::Builtin(Builtin::Mul).with_span(star.span()))
                }
                ast::ExprSymbol::Percent(percent) => {
                    Ok(Expr::Builtin(Builtin::Rem).with_span(percent.span()))
                }
                ast::ExprSymbol::Slash(slash) => {
                    Ok(Expr::Builtin(Builtin::Div).with_span(slash.span()))
                }
                ast::ExprSymbol::Dot(_dot) => todo!("lower `.`"),
                ast::ExprSymbol::DotDot(_dotdot) => todo!("lower `..`"),
                ast::ExprSymbol::Semi(_semi) => todo!("lower `;`"),
                ast::ExprSymbol::Equal(equal) => {
                    Ok(Expr::Builtin(Builtin::Eq).with_span(equal.span()))
                }
                ast::ExprSymbol::Less(less) => {
                    Ok(Expr::Builtin(Builtin::Lt).with_span(less.span()))
                }
                ast::ExprSymbol::Greater(greater) => {
                    Ok(Expr::Builtin(Builtin::Gt).with_span(greater.span()))
                }
                ast::ExprSymbol::LessEqual(less_equal) => {
                    Ok(Expr::Builtin(Builtin::Le).with_span(less_equal.span()))
                }
                ast::ExprSymbol::GreaterEqual(greater_equal) => {
                    Ok(Expr::Builtin(Builtin::Ge).with_span(greater_equal.span()))
                }
            },
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Expr::I32(int).with_span(integer.span())),
                    Err(_) => {
                        self.errors.push(LowerError::from(integer));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    if let Some(ty) = self.type_of(ident.literal) {
                        Ok(Expr::Ident {
                            literal: ident.literal,
                            ty,
                        }
                        .with_span(ident.span()))
                    } else {
                        self.errors.push(LowerError::from(ident));
                        Err(PushedErrors)
                    }
                }
                ast::ExprLit::True(t) => Ok(Expr::Bool(true).with_span(t.span())),
                ast::ExprLit::False(f) => Ok(Expr::Bool(false).with_span(f.span())),
            },
            ast::Expr::Tuple(tuple) => {
                fn rec<'ast, 'hir, 'input: 'ast>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    mut exprs: impl Iterator<Item = &'ast ast::Expr<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir List<'hir, S<Expr<'hir, 'input>>>,
                        &'hir List<'hir, S<Type<'hir>>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(ast_expr) = exprs.next() else {
                        return Ok(None);
                    };

                    let hir_expr = scope.lower(ast_expr)?;

                    let (next_expr, next_type) = rec(scope, exprs)?.unzip();

                    Ok(Some((
                        scope.hir.list_exprs.alloc(List {
                            item: hir_expr,
                            next: next_expr,
                        }),
                        scope.hir.list_types.alloc(List {
                            item: hir_expr.ty(),
                            next: next_type,
                        }),
                    )))
                }

                if let Some((exprs, ty)) = rec(self, tuple.iter_elements().copied())? {
                    Ok(Expr::Tuple {
                        ty: Type::Tuple(ty).with_span(tuple.span()),
                        exprs,
                    }
                    .with_span(tuple.span()))
                } else {
                    Ok(Expr::Unit.with_span(tuple.span()))
                }
            }
            ast::Expr::Closure(closure) => {
                let mut inner = self.enter_scope();

                let [lhs, rhs] = inner.type_of_many_params(&closure.head)?;
                let body = inner.lower(closure.head.body)?;

                drop(inner);

                fn rec<'ast, 'hir, 'input: 'ast>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    head: &ExprArm<'hir, 'input>,
                    mut branches: impl Iterator<Item = &'ast ast::ExprArm<'ast, 'input>>,
                ) -> Result<Option<&'hir List<'hir, ExprArm<'hir, 'input>>>, PushedErrors>
                {
                    let Some(branch) = branches.next() else {
                        return Ok(None);
                    };

                    let mut inner = scope.enter_scope();
                    let [lhs, rhs] = inner.type_of_many_params(branch)?;
                    let body = inner.lower(branch.body)?;
                    drop(inner);

                    scope.unify(head.lhs.ty(), lhs.ty());
                    scope.unify(head.rhs.ty(), rhs.ty());
                    scope.unify(head.body.ty(), body.ty());

                    if scope.had_errors() {
                        Err(PushedErrors)
                    } else {
                        let next = rec(scope, head, branches)?;
                        Ok(Some(scope.hir.list_expr_branches.alloc(List {
                            item: ExprArm { lhs, rhs, body },
                            next,
                        })))
                    }
                }

                let head = ExprArm { lhs, rhs, body };

                let next = rec(self, &head, closure.tail.iter().map(|(_, arm)| arm))?;

                let arms = self.hir.list_expr_branches.alloc(List { item: head, next });

                let ty = Type::Function(self.hir.type_functions.alloc(TypeFunction {
                    lhs: head.lhs.ty(),
                    rhs: head.rhs.ty(),
                    output: head.body.ty(),
                }))
                .with_span(closure.head.span());

                Ok(Expr::Closure { ty, arms }.with_span(closure.span()))
            }
            ast::Expr::Appl(appl) => {
                let lhs = self.lower(appl.lhs);
                let rhs = self.lower(appl.rhs);
                let function = self.lower(appl.function);

                let (Ok(lhs), Ok(rhs), Ok(function)) = (lhs, rhs, function) else {
                    return Err(PushedErrors);
                };

                let ty = Type::Var(self.hir.new_typevar()).with_span(appl.span());

                let expected_function = self.hir.type_functions.alloc(TypeFunction {
                    lhs: lhs.ty(),
                    rhs: rhs.ty(),
                    output: ty,
                });

                self.unify(
                    function.ty(),
                    Type::Function(expected_function).with_span(appl.function.span()),
                );

                if self.had_errors() {
                    Err(PushedErrors)
                } else {
                    Ok(Expr::Appl {
                        ty,
                        appl: self.hir.expr_appls.alloc(ExprAppl { lhs, function, rhs }),
                    }
                    .with_span(appl.span()))
                }
            }
        }
    }

    /// Returns the [`Type<'hir>`] of an [`ast::ExprPat`].
    fn lower_pat(
        &mut self,
        pat: &ast::ExprPat<'_, 'input>,
    ) -> Result<S<Pat<'hir, 'input>>, PushedErrors> {
        match pat {
            ast::Pat::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Pat::I32(int).with_span(integer.span())),
                    Err(_) => {
                        self.errors.push(LowerError::from(integer));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    let ty = Type::Var(self.hir.new_typevar()).with_span(ident.span());
                    self.add_local(ident.literal, ty);
                    Ok(Pat::Ident {
                        literal: ident.literal,
                        ty,
                    }
                    .with_span(ident.span()))
                }
                ast::ExprLit::True(_) => Ok(Pat::Bool(true).with_span(lit.span())),
                ast::ExprLit::False(_) => Ok(Pat::Bool(false).with_span(lit.span())),
            },
            ast::Pat::Tuple(tuple) => {
                // TODO(quinn): this is mostly copy pasted from `Env::lower`,
                // can we try to generalize them? Patterns are basically just
                // expressions without closures or function application...
                fn rec<'ast, 'hir, 'input: 'ast>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    mut pats: impl Iterator<Item = &'ast ast::ExprPat<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir List<'hir, S<Pat<'hir, 'input>>>,
                        &'hir List<'hir, S<Type<'hir>>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(pat) = pats.next() else {
                        return Ok(None);
                    };

                    let pat = scope.lower_pat(pat)?;

                    let (next_pat, next_type) = rec(scope, pats)?.unzip();

                    Ok(Some((
                        &*scope.hir.list_pats.alloc(List {
                            item: pat,
                            next: next_pat,
                        }),
                        scope.hir.list_types.alloc(List {
                            item: pat.ty(),
                            next: next_type,
                        }),
                    )))
                }

                if let Some((exprs, ty)) = rec(self, tuple.iter_elements().copied())? {
                    Ok(Pat::Tuple {
                        ty: Type::Tuple(ty).with_span(tuple.span()),
                        pats: exprs,
                    }
                    .with_span(tuple.span()))
                } else {
                    Ok(Pat::Unit.with_span(tuple.span()))
                }
            } // When we add struct destructuring, we can unify the type of the field
              // with the returned type of the pattern in that field.
              // e.g. If we have a `struct Number(i32)` and have the pattern
              // `Number(x)`, then `x` might be type `Var(Typevar::Unbound(0))` which
              // we can unify with `i32` to see that it should be `Var(Typevar::Bound(&Int))`
        }
    }

    /// Returns the [`Type<'hir>`] of a single [`ast::ExprParam`].
    fn lower_param(
        &mut self,
        param: &ast::ExprParam<'_, 'input>,
    ) -> Result<S<Pat<'hir, 'input>>, PushedErrors> {
        let pat = self.lower_pat(param.pat)?;
        if let Some((_, annotation)) = param.ty {
            let t2 = self.hir.type_from_ast(annotation, self.type_map);
            self.unify(pat.ty(), t2);
            if self.had_errors() {
                return Err(PushedErrors);
            }
        }

        Ok(pat)
    }

    /// Returns the [`Type<'hir>`]s of various [`ast::ExprParams`].
    fn type_of_many_params(
        &mut self,
        arm: &ast::ExprArm<'_, 'input>,
    ) -> Result<[S<Pat<'hir, 'input>>; 2], PushedErrors> {
        match &arm.params {
            ast::ExprParams::Zero => Ok([Pat::Unit.with_span(arm.close.span()); 2]),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(lhs);
                Ok([lhs_type?, Pat::Unit.with_span(arm.close.span())])
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(lhs);
                let rhs_type = self.lower_param(rhs);
                Ok([lhs_type?, rhs_type?])
            }
        }
    }

    /// Unify two types.
    pub fn unify(&mut self, t1: S<Type<'hir>>, t2: S<Type<'hir>>) -> NodeIndex {
        match (t1, t2) {
            (S(Type::I32, _), S(Type::I32, _)) => self.hir.equations.add_rule(Node::Equiv(t1, t2)),
            (S(Type::Bool, _), S(Type::Bool, _)) => {
                self.hir.equations.add_rule(Node::Equiv(t1, t2))
            }
            (S(Type::Unit, _), S(Type::Unit, _)) => {
                self.hir.equations.add_rule(Node::Equiv(t1, t2))
            }
            (S(Type::Tuple(a), _), S(Type::Tuple(b), _)) => {
                if a.len() == b.len() {
                    let conclusion = self.hir.equations.add_rule(Node::Equiv(t1, t2));

                    for (i, (&t1, &t2)) in a.iter().zip(b.iter()).enumerate() {
                        let mut inner = self.enter_scope();
                        let proof = inner.unify(t1, t2);
                        if inner.had_errors() {
                            inner.hir.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                        }
                        inner
                            .hir
                            .equations
                            .add_proof(proof, conclusion, Edge::Tuple(i));
                    }
                    conclusion
                } else {
                    // different length tuples
                    self.errors.push(LowerError::unify(t1, t2));
                    self.hir.equations.add_rule(Node::NotEquiv(t1, t2))
                }
            }
            (S(Type::Var(var), var_span), a) | (a, S(Type::Var(var), var_span)) => {
                if let Some((b, _binding_source)) = self.hir[var] {
                    let proof = self.unify(a, b);

                    let conclusion = if self.had_errors() {
                        self.hir.equations.add_rule(Node::NotEquiv(t1, t2))
                    } else {
                        self.hir.equations.add_rule(Node::Equiv(t1, t2))
                    };
                    // If we wanted, we could also add an edge with `_binding_source`,
                    // which tells us exactly where the typevar was bound.
                    self.hir
                        .equations
                        .add_proof(proof, conclusion, Edge::Transitivity);
                    conclusion
                } else if self.hir.check_equivalence(var, a) {
                    self.hir.equations.add_rule(Node::Equiv(t1, t2))
                } else if self.hir.occurs(var, a) {
                    self.errors.push(LowerError::CyclicType {
                        var_span,
                        var,
                        ty_span: a.span(),
                        ty: *a.value(),
                    });
                    self.hir.equations.add_rule(Node::NotEquiv(t1, t2))
                } else {
                    // The actual binding code is here
                    let conclusion = self
                        .hir
                        .equations
                        .add_rule(Node::Binding { var, definition: a });

                    self.hir[var] = Some((a, conclusion));
                    conclusion
                }
            }
            (S(Type::Function(f1), _), S(Type::Function(f2), _)) => {
                let conclusion = self.hir.equations.add_rule(Node::Equiv(t1, t2));
                let lhs_proof = self.unify(f1.lhs, f2.lhs);
                let rhs_proof = self.unify(f1.rhs, f2.rhs);
                let output_proof = self.unify(f1.output, f2.output); // here

                if self.had_errors() {
                    self.hir.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                }

                self.hir
                    .equations
                    .add_proof(lhs_proof, conclusion, Edge::FunctionLhs);
                self.hir
                    .equations
                    .add_proof(rhs_proof, conclusion, Edge::FunctionRhs);
                self.hir
                    .equations
                    .add_proof(output_proof, conclusion, Edge::FunctionOutput);

                conclusion
            }
            _ => {
                self.errors.push(LowerError::unify(t1, t2));
                self.hir.equations.add_rule(Node::NotEquiv(t1, t2))
            }
        }
    }
}

impl Drop for Scope<'_, '_, '_> {
    fn drop(&mut self) {
        self.locals.truncate(self.original_locals_len);
    }
}

pub struct Hir<'hir, 'input> {
    pub type_functions: &'hir Arena<TypeFunction<'hir>>,
    pub expr_appls: &'hir Arena<ExprAppl<'hir, 'input>>,
    pub list_expr_branches: &'hir Arena<List<'hir, ExprArm<'hir, 'input>>>,
    pub list_exprs: &'hir Arena<List<'hir, S<Expr<'hir, 'input>>>>,
    pub list_types: &'hir Arena<List<'hir, S<Type<'hir>>>>,
    pub list_pats: &'hir Arena<List<'hir, S<Pat<'hir, 'input>>>>,
    pub typevars: Vec<Typevar<'hir>>,
    pub equations: Equations<'hir>,
}

impl<'hir, 'input> Hir<'hir, 'input> {
    pub fn new_typevar(&mut self) -> Var {
        let var = Var(self.typevars.len());
        self.typevars.push(None);
        var
    }

    /// Get a global environment with the type signatures of `in` and `print`
    /// already loaded in.
    ///
    /// `in`: `x (x () -> y) -> y`
    ///
    /// and
    ///
    /// `print`: `x () -> ()`
    pub fn default_globals(&mut self) -> impl Iterator<Item = (&'static str, Polytype<'hir>)> {
        let dummy = (0, 0);
        [
            ("in", {
                let x = self.new_typevar();
                let x_type = Type::Var(x).with_span(dummy);
                let y = self.new_typevar();
                let y_type = Type::Var(x).with_span(dummy);

                Polytype {
                    typevars: smallvec![x, y],
                    ty: Type::Function(
                        self.type_functions.alloc(TypeFunction {
                            lhs: x_type,
                            rhs: Type::Function(self.type_functions.alloc(TypeFunction {
                                lhs: x_type,
                                rhs: Type::Unit.with_span(dummy),
                                output: y_type,
                            }))
                            .with_span(dummy),
                            output: y_type,
                        }),
                    )
                    .with_span(dummy),
                }
            }),
            ("print", {
                let x = self.new_typevar();
                let x_type = Type::Var(x).with_span((0, 0));

                Polytype {
                    typevars: smallvec![x],
                    ty: Type::Function(self.type_functions.alloc(TypeFunction {
                        lhs: x_type,
                        rhs: Type::Unit.with_span((0, 0)),
                        output: Type::Unit.with_span((0, 0)),
                    }))
                    .with_span(dummy),
                }
            }),
        ]
        .into_iter()
    }

    pub fn monomorphize(&mut self, polytype: &Polytype<'hir>) -> S<Type<'hir>> {
        // Takes a polymorphic type and replaces all instances of generics
        // with a fixed, unbound type.
        // For example, id: T -> T is a polymorphic type, so it goes through
        // and replaces both `T`s with an unbound type variable like `a0`,
        // which is then bound later on.
        fn replace_unbound_typevars<'hir>(
            tbl: &HashMap<Var, Type<'hir>>,
            hir: &mut Hir<'hir, '_>,
            ty: S<Type<'hir>>,
        ) -> S<Type<'hir>> {
            match ty.value() {
                Type::Var(var) => {
                    tbl[&var].with_span(ty.span()) // generics should always be in the map
                }
                Type::Function(fun) => Type::Function(hir.type_functions.alloc(TypeFunction {
                    lhs: replace_unbound_typevars(tbl, hir, fun.lhs),
                    rhs: replace_unbound_typevars(tbl, hir, fun.rhs),
                    output: replace_unbound_typevars(tbl, hir, fun.output),
                }))
                .with_span(ty.span()),
                Type::Tuple(types) => {
                    fn rec<'hir>(
                        tbl: &HashMap<Var, Type<'hir>>,
                        hir: &mut Hir<'hir, '_>,
                        types: &'hir List<'hir, S<Type<'hir>>>,
                    ) -> &'hir List<'hir, S<Type<'hir>>> {
                        let next = types.next.map(|next| rec(tbl, hir, next));
                        hir.list_types.alloc(List {
                            item: replace_unbound_typevars(tbl, hir, types.item),
                            next,
                        })
                    }
                    Type::Tuple(rec(tbl, hir, types)).with_span(ty.span())
                }
                _ => ty,
            }
        }

        let tvs_to_replace = polytype
            .typevars
            .iter()
            .map(|tv| (*tv, Type::Var(self.new_typevar())))
            .collect();

        replace_unbound_typevars(&tvs_to_replace, self, polytype.ty)
    }

    /// Convert an [`ast::Type<'_, 'input>`] annotation into an HIR [`Type<'hir>`].
    pub fn type_from_ast(
        &mut self,
        ty: &ast::Type<'_, 'input>,
        map: &HashMap<&str, S<Type<'hir>>>,
    ) -> S<Type<'hir>> {
        match ty {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => Type::I32.with_span(named.span()),
                "bool" => Type::Bool.with_span(named.span()),
                other => map.get(other).copied().expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                // Build up the linked list of types from the inside out
                fn rec<'hir, 'ast, 'input: 'ast>(
                    hir: &mut Hir<'hir, 'input>,
                    map: &HashMap<&str, S<Type<'hir>>>,
                    mut types: impl Iterator<Item = &'ast ast::Type<'ast, 'input>>,
                ) -> Option<&'hir List<'hir, S<Type<'hir>>>> {
                    let item = hir.type_from_ast(types.next()?, map);
                    let next = rec(hir, map, types);
                    Some(hir.list_types.alloc(List { item, next }))
                }

                if let Some(ty) = rec(self, map, tuple.iter_elements().copied()) {
                    Type::Tuple(ty).with_span(tuple.span())
                } else {
                    Type::Unit.with_span(tuple.span())
                }
            }
            ast::Type::Function(fun) => {
                let lhs = self.type_from_ast(fun.lhs, map);
                let rhs = self.type_from_ast(fun.rhs, map);
                let output = self.type_from_ast(fun.ret, map);

                Type::Function(self.type_functions.alloc(TypeFunction { lhs, rhs, output }))
                    .with_span(fun.span())
            }
        }
    }

    fn occurs(&self, var: Var, ty: S<Type<'_>>) -> bool {
        match *ty.value() {
            Type::Var(typevar) => {
                if let Some((t, _)) = self[typevar] {
                    self.occurs(var, t)
                } else {
                    var == typevar
                }
            }
            Type::Function(fun) => {
                self.occurs(var, fun.lhs)
                    || self.occurs(var, fun.rhs)
                    || self.occurs(var, fun.output)
            }
            _ => false,
        }
    }

    fn check_equivalence(&self, var: Var, ty: S<Type<'_>>) -> bool {
        if let S(Type::Var(typevar), _) = ty {
            if let Some((t, _)) = self[typevar] {
                self.check_equivalence(var, t)
            } else {
                var == typevar
            }
        } else {
            false
        }
    }
}

impl<'hir> Index<Var> for Hir<'hir, '_> {
    type Output = Typevar<'hir>;

    fn index(&self, index: Var) -> &Self::Output {
        &self.typevars[index.0 as usize]
    }
}

impl<'hir> IndexMut<Var> for Hir<'hir, '_> {
    fn index_mut(&mut self, index: Var) -> &mut Self::Output {
        &mut self.typevars[index.0 as usize]
    }
}

#[derive(Debug)]
pub struct PushedErrors;
