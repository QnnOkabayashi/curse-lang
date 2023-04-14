use curse_ast as ast;
use displaydoc::Display;
use petgraph::graph::NodeIndex;
use std::{
    collections::HashMap,
    fmt, num,
    ops::{Index, IndexMut},
};
use thiserror::Error;
use typed_arena::Arena;

mod equations;
use equations::{Edge, Equations, Node};
mod expr;
use expr::*;
mod dot;

#[cfg(test)]
mod tests;

/// `Some` is bound, `None` is unbound.
pub type Typevar<'hir> = Option<(Type<'hir>, NodeIndex)>;

#[derive(Copy, Clone, Display, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("T{0}")]
pub struct Var(usize);

#[derive(Copy, Clone)]
pub enum Type<'hir> {
    I32,
    Bool,
    Unit,
    Var(Var),
    Tuple(&'hir List<'hir, Type<'hir>>),
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
                if let Some(item) = iter.next() {
                    write!(f, "{}", item.pretty(self.hir))?;
                }
                for item in iter {
                    write!(f, ", {}", item.pretty(self.hir))?;
                }
                write!(f, ")")
            }
            Type::Var(var) => {
                if let Some((ty, _)) = &self.hir[var] {
                    write!(f, "{}", ty.pretty(self.hir))
                } else {
                    write!(f, "{var}")
                }
            }
            Type::Function(fun) => {
                write!(
                    f,
                    "({} {} -> {})",
                    fun.lhs.pretty(self.hir),
                    fun.rhs.pretty(self.hir),
                    fun.output.pretty(self.hir)
                )
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("({lhs} {rhs} -> {output})")]
pub struct TypeFunction<'hir> {
    lhs: Type<'hir>,
    rhs: Type<'hir>,
    output: Type<'hir>,
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
    /// Returns a [`Display`]able type with a provided delimiter.
    pub fn delim<'a>(&'a self, delim: &'a str) -> Delim<'a, T> {
        Delim { list: self, delim }
    }
}

impl<T: fmt::Debug> fmt::Debug for List<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
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

#[derive(Clone, Debug)]
pub struct Polytype<'hir> {
    typevars: Vec<Var>,
    typ: Type<'hir>,
}

impl<'hir> Polytype<'hir> {
    pub fn new(ty: Type<'hir>) -> Self {
        Polytype {
            typevars: vec![],
            typ: ty,
        }
    }
}

pub struct Scope<'outer, 'hir, 'input> {
    hir: &'outer mut Hir<'hir, 'input>,
    type_map: &'outer HashMap<&'outer str, Type<'hir>>,
    errors: &'outer mut Vec<LowerError<'hir, 'input>>,
    original_errors_len: usize,
    globals: &'hir HashMap<&'hir str, Polytype<'hir>>,
    locals: &'outer mut Vec<(&'hir str, Type<'hir>)>,
    original_locals_len: usize,
}

impl<'outer, 'hir, 'input: 'hir> Scope<'outer, 'hir, 'input> {
    pub fn new(
        hir: &'outer mut Hir<'hir, 'input>,
        type_map: &'outer HashMap<&'outer str, Type<'hir>>,
        errors: &'outer mut Vec<LowerError<'hir, 'input>>,
        globals: &'hir HashMap<&'hir str, Polytype>,
        locals: &'outer mut Vec<(&'hir str, Type<'hir>)>,
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
    pub fn type_of(&mut self, var: &str) -> Option<Type<'hir>> {
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

    pub fn add_local(&mut self, var: &'hir str, ty: Type<'hir>) {
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
    ) -> Result<Expr<'hir, 'input>, PushedErrors> {
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
                    Err(source) => {
                        self.errors.push(LowerError::ParseInt {
                            token: *integer,
                            source,
                        });
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
                            .push(LowerError::IdentNotFound { token: *ident });
                        Err(PushedErrors)
                    }
                }
                ast::ExprLit::True(_) => Ok(Expr::Bool(true)),
                ast::ExprLit::False(_) => Ok(Expr::Bool(false)),
            },
            ast::Expr::Tuple(tuple) => {
                fn rec<'ast, 'hir, 'input: 'ast>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    mut exprs: impl Iterator<Item = &'ast ast::Expr<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir List<'hir, Expr<'hir, 'input>>,
                        &'hir List<'hir, Type<'hir>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(expr) = exprs.next() else {
                        return Ok(None);
                    };

                    let expr = scope.lower(expr)?;

                    let (next_expr, next_type) = rec(scope, exprs)?.unzip();

                    Ok(Some((
                        scope.hir.list_exprs.alloc(List {
                            item: expr,
                            next: next_expr,
                        }),
                        scope.hir.list_types.alloc(List {
                            item: expr.ty(),
                            next: next_type,
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

                let [lhs, rhs] = inner.type_of_many_params(&closure.head.params)?;
                let body = inner.lower(closure.head.body)?;

                drop(inner);

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
                    let [lhs, rhs] = inner.type_of_many_params(&branch.params)?;
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
                            item: ExprBranch { lhs, rhs, body },
                            next,
                        })))
                    }
                }

                let head = ExprBranch { lhs, rhs, body };

                let next = rec(self, &head, closure.tail.iter().map(|(_, branch)| branch))?;

                let branches = self.hir.list_expr_branches.alloc(List { item: head, next });

                let ty = Type::Function(self.hir.type_functions.alloc(TypeFunction {
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

                let ty = self.hir.new_typevar().1;

                let index_of_function = self.hir.type_functions.alloc(TypeFunction {
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
                        appl: self.hir.expr_appls.alloc(ExprAppl { lhs, function, rhs }),
                    })
                }
            }
        }
    }

    /// Returns the [`Type<'hir>`] of an [`ast::ExprPat`].
    fn lower_pat(
        &mut self,
        pat: &ast::ExprPat<'_, 'input>,
    ) -> Result<Pat<'hir, 'input>, PushedErrors> {
        match pat {
            ast::Pat::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Pat::I32(int)),
                    Err(source) => {
                        self.errors.push(LowerError::ParseInt {
                            token: *integer,
                            source,
                        });
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    let ty = self.hir.new_typevar().1;
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
                fn rec<'ast, 'hir, 'input: 'ast>(
                    scope: &mut Scope<'_, 'hir, 'input>,
                    mut pats: impl Iterator<Item = &'ast ast::ExprPat<'ast, 'input>>,
                ) -> Result<
                    Option<(
                        &'hir List<'hir, Pat<'hir, 'input>>,
                        &'hir List<'hir, Type<'hir>>,
                    )>,
                    PushedErrors,
                > {
                    let Some(pat) = pats.next() else {
                        return Ok(None);
                    };

                    let pat = scope.lower_pat(pat)?;

                    let (next_pat, next_type) = rec(scope, pats)?.unzip();

                    Ok(Some((
                        scope.hir.list_pats.alloc(List {
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
                        ty: Type::Tuple(ty),
                        pats: exprs,
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

    /// Returns the [`Type<'hir>`] of a single [`ast::ExprParam`].
    fn lower_param(
        &mut self,
        param: &ast::ExprParam<'_, 'input>,
    ) -> Result<Pat<'hir, 'input>, PushedErrors> {
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
        params: &ast::ExprParams<'_, 'input>,
    ) -> Result<[Pat<'hir, 'input>; 2], PushedErrors> {
        match params {
            ast::ExprParams::Zero => Ok([Pat::Unit, Pat::Unit]),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(lhs);
                Ok([lhs_type?, Pat::Unit])
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(lhs);
                let rhs_type = self.lower_param(rhs);
                Ok([lhs_type?, rhs_type?])
            }
        }
    }

    /// Unify two types.
    fn unify(&mut self, t1: Type<'hir>, t2: Type<'hir>) -> NodeIndex {
        match (t1, t2) {
            (Type::I32, Type::I32) => self.hir.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Bool, Type::Bool) => self.hir.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Unit, Type::Unit) => self.hir.equations.add_rule(Node::Equiv(t1, t2)),
            (Type::Tuple(a), Type::Tuple(b)) => {
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
                    self.errors.push(LowerError::Unify(t1, t2));
                    self.hir.equations.add_rule(Node::NotEquiv(t1, t2))
                }
            }
            (Type::Var(var), a) | (a, Type::Var(var)) => {
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
                    self.errors.push(LowerError::CyclicType(var, a));
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
            (Type::Function(f1), Type::Function(f2)) => {
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
                self.errors.push(LowerError::Unify(t1, t2));
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

#[derive(Clone, Debug, Error)]
pub enum LowerError<'hir, 'input> {
    #[error("Cannot unify types: {0} and {1}")]
    Unify(Type<'hir>, Type<'hir>),
    #[error("Cyclic type: var is {0}, while type is {1}")]
    CyclicType(Var, Type<'hir>),
    #[error("Identifier not found: `{token}`")]
    IdentNotFound { token: ast::tok::Ident<'input> },
    #[error("Cannot parse integer {token}: {source}")]
    ParseInt {
        token: ast::tok::Integer<'input>,
        source: num::ParseIntError,
    },
}

pub struct Hir<'hir, 'input> {
    type_functions: &'hir Arena<TypeFunction<'hir>>,
    expr_appls: &'hir Arena<ExprAppl<'hir, 'input>>,
    list_expr_branches: &'hir Arena<List<'hir, ExprBranch<'hir, 'input>>>,
    list_exprs: &'hir Arena<List<'hir, Expr<'hir, 'input>>>,
    list_types: &'hir Arena<List<'hir, Type<'hir>>>,
    list_pats: &'hir Arena<List<'hir, Pat<'hir, 'input>>>,
    typevars: Vec<Typevar<'hir>>,
    equations: Equations<'hir>,
}

impl<'hir, 'input> Hir<'hir, 'input> {
    fn new_typevar(&mut self) -> (Var, Type<'hir>) {
        let var = Var(self.typevars.len());
        self.typevars.push(None);
        (var, Type::Var(var))
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
        [
            ("in", {
                let (x, x_type) = self.new_typevar();
                let (y, y_type) = self.new_typevar();

                Polytype {
                    typevars: vec![x, y],
                    typ: Type::Function(self.type_functions.alloc(TypeFunction {
                        lhs: x_type,
                        rhs: Type::Function(self.type_functions.alloc(TypeFunction {
                            lhs: x_type,
                            rhs: Type::Unit,
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
                    typ: Type::Function(self.type_functions.alloc(TypeFunction {
                        lhs: x_type,
                        rhs: Type::Unit,
                        output: Type::Unit,
                    })),
                }
            }),
        ]
        .into_iter()
    }

    pub fn monomorphize(&mut self, polytype: &Polytype<'hir>) -> Type<'hir> {
        // Takes a polymorphic type and replaces all instances of generics
        // with a fixed, unbound type.
        // For example, id: T -> T is a polymorphic type, so it goes through
        // and replaces both `T`s with an unbound type variable like `a0`,
        // which is then bound later on.
        fn replace_unbound_typevars<'hir>(
            tbl: &HashMap<Var, Type<'hir>>,
            hir: &mut Hir<'hir, '_>,
            ty: Type<'hir>,
        ) -> Type<'hir> {
            match ty {
                Type::Var(var) => {
                    tbl[&var] // generics should always be in the map
                }
                Type::Function(fun) => Type::Function(hir.type_functions.alloc(TypeFunction {
                    lhs: replace_unbound_typevars(tbl, hir, fun.lhs),
                    rhs: replace_unbound_typevars(tbl, hir, fun.rhs),
                    output: replace_unbound_typevars(tbl, hir, fun.output),
                })),
                Type::Tuple(types) => {
                    fn rec<'hir>(
                        tbl: &HashMap<Var, Type<'hir>>,
                        hir: &mut Hir<'hir, '_>,
                        types: &'hir List<'hir, Type<'hir>>,
                    ) -> &'hir List<'hir, Type<'hir>> {
                        let next = types.next.map(|next| rec(tbl, hir, next));
                        hir.list_types.alloc(List {
                            item: replace_unbound_typevars(tbl, hir, types.item),
                            next,
                        })
                    }
                    Type::Tuple(rec(tbl, hir, types))
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

    /// Convert an [`ast::Type<'_, 'input>`] annotation into an HIR [`Type<'hir>`].
    pub fn type_from_ast(
        &mut self,
        typ: &ast::Type<'_, 'input>,
        map: &HashMap<&str, Type<'hir>>,
    ) -> Type<'hir> {
        match typ {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => Type::I32,
                "bool" => Type::Bool,
                other => map.get(other).copied().expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                // Build up the linked list of types from the inside out
                fn rec<'hir, 'ast, 'input: 'ast>(
                    hir: &mut Hir<'hir, 'input>,
                    map: &HashMap<&str, Type<'hir>>,
                    mut types: impl Iterator<Item = &'ast ast::Type<'ast, 'input>>,
                ) -> Option<&'hir List<'hir, Type<'hir>>> {
                    let item = hir.type_from_ast(types.next()?, map);
                    let next = rec(hir, map, types);
                    Some(hir.list_types.alloc(List { item, next }))
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

                Type::Function(self.type_functions.alloc(TypeFunction { lhs, rhs, output }))
            }
        }
    }

    fn occurs(&self, var: Var, ty: Type<'_>) -> bool {
        match ty {
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

    fn check_equivalence(&self, var: Var, ty: Type<'_>) -> bool {
        if let Type::Var(typevar) = ty {
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
