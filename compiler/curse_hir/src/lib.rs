#![allow(dead_code)]
use ast::Span;
use curse_ast as ast;
use displaydoc::Display;
use petgraph::graph::NodeIndex;
use smallvec::{smallvec, SmallVec};
use std::{
    collections::HashMap,
    fmt, iter,
    ops::{Index, IndexMut},
};
use thiserror::Error;
use typed_arena::Arena;

mod equations;
pub use equations::{Edge, Equations, Node};
mod expr;
pub use expr::*;
pub mod dot;
// todo: move typeck to another module
mod error;
pub use error::*;
pub mod usefulness;

pub enum Typevar<'hir> {
    /// An unbound type variable
    Unbound,
    // A bound type variable.
    Bound {
        ty: Type<'hir>,
        source: NodeIndex,
    },
}

impl<'hir> Typevar<'hir> {
    fn binding(&self) -> Option<&Type<'hir>> {
        match self {
            Typevar::Bound { ty, .. } => Some(ty),
            Typevar::Unbound => None,
        }
    }
}

#[derive(Copy, Clone, Display, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("T{0}")]
pub struct Var(usize);

#[derive(Debug, Error)]
#[error("Unbound typevar")]
pub struct UnboundTypevar;

#[derive(Copy, Clone, Debug, Display)]
#[displaydoc("{kind}")]
pub struct Type<'hir> {
    pub kind: TypeKind<'hir>,
    pub span: (usize, usize),
}

impl<'hir> Type<'hir> {
    fn dummy() -> Self {
        Type {
            kind: TypeKind::unit(),
            span: (0, 0),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TypeKind<'hir> {
    I32,
    Bool,
    Var(Var),
    Tuple(&'hir [Type<'hir>]),
    Function(&'hir TypeFunction<'hir>),
}

impl<'hir> TypeKind<'hir> {
    pub fn unit() -> Self {
        TypeKind::Tuple(&[])
    }

    /// Returns a [`Display`](fmt::Display)able type that prints a [`Type`],
    /// except with all type variables fully expanded as much as possible.
    pub fn pretty(self, hir: &'hir Hir<'hir, 'hir>) -> TypePrinter<'hir> {
        TypePrinter { ty: self, hir }
    }

    pub fn resolve(&self, hir: &Hir<'hir, '_>) -> Result<Self, UnboundTypevar> {
        let TypeKind::Var(var) = self else {
            return Ok(*self);
        };

        hir[*var].binding().ok_or(UnboundTypevar)?.kind.resolve(hir)
    }
}

impl fmt::Display for TypeKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Tuple(types) => {
                f.write_str("(")?;
                if let Some((head, tail)) = types.split_first() {
                    write!(f, "{head}")?;
                    for ty in tail {
                        write!(f, ", {ty}")?;
                    }
                }
                f.write_str(")")
            }
            TypeKind::Var(var) => write!(f, "{var}"),
            TypeKind::Function(fun) => write!(f, "{fun}"),
        }
    }
}

pub struct TypePrinter<'a> {
    ty: TypeKind<'a>,
    hir: &'a Hir<'a, 'a>,
}

impl fmt::Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Tuple(tuple) => {
                write!(f, "(")?;
                let mut iter = tuple.iter();
                if let Some(ty) = iter.next() {
                    write!(f, "{}", ty.kind.pretty(self.hir))?;
                }
                for ty in iter {
                    write!(f, ", {}", ty.kind.pretty(self.hir))?;
                }
                write!(f, ")")
            }
            TypeKind::Var(var) => {
                if let Some(ty) = self.hir[var].binding() {
                    write!(f, "{}", ty.kind.pretty(self.hir))
                } else {
                    write!(f, "{var}")
                }
            }
            TypeKind::Function(fun) => {
                write!(
                    f,
                    "({} {} -> {})",
                    fun.lhs.kind.pretty(self.hir),
                    fun.rhs.kind.pretty(self.hir),
                    fun.output.kind.pretty(self.hir)
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

#[derive(Clone, Debug)]
pub struct Polytype<'hir> {
    pub typevars: SmallVec<[Var; 4]>,
    pub ty: Type<'hir>,
}

impl<'hir> Polytype<'hir> {
    pub fn new(ty: Type<'hir>) -> Self {
        Polytype {
            typevars: SmallVec::new(),
            ty,
        }
    }
}

pub struct Scope<'outer, 'hir, 'input> {
    pub hir: &'outer mut Hir<'hir, 'input>,
    type_map: &'outer HashMap<&'outer str, Type<'hir>>,
    errors: &'outer mut Vec<LowerError<'hir>>,
    original_errors_len: usize,
    globals: &'hir HashMap<&'hir str, Polytype<'hir>>,
    locals: &'outer mut Vec<(&'hir str, Type<'hir>)>,
    original_locals_len: usize,
}

impl<'outer, 'hir, 'input: 'hir> Scope<'outer, 'hir, 'input> {
    pub fn new(
        hir: &'outer mut Hir<'hir, 'input>,
        type_map: &'outer HashMap<&'outer str, Type<'hir>>,
        errors: &'outer mut Vec<LowerError<'hir>>,
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
                ast::ExprSymbol::Plus(plus) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Add),
                    span: plus.span(),
                }),
                ast::ExprSymbol::Minus(minus) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Sub),
                    span: minus.span(),
                }),
                ast::ExprSymbol::Star(star) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Mul),
                    span: star.span(),
                }),
                ast::ExprSymbol::Percent(percent) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Rem),
                    span: percent.span(),
                }),
                ast::ExprSymbol::Slash(slash) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Div),
                    span: slash.span(),
                }),
                ast::ExprSymbol::Dot(_dot) => todo!("lower `.`"),
                ast::ExprSymbol::DotDot(_dotdot) => todo!("lower `..`"),
                ast::ExprSymbol::Semi(_semi) => todo!("lower `;`"),
                ast::ExprSymbol::Equal(equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Eq),
                    span: equal.span(),
                }),
                ast::ExprSymbol::Less(less) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Lt),
                    span: less.span(),
                }),
                ast::ExprSymbol::Greater(greater) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Gt),
                    span: greater.span(),
                }),
                ast::ExprSymbol::LessEqual(less_equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Le),
                    span: less_equal.span(),
                }),
                ast::ExprSymbol::GreaterEqual(greater_equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Ge),
                    span: greater_equal.span(),
                }),
            },
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Expr {
                        kind: ExprKind::I32(int),
                        span: integer.span(),
                    }),
                    Err(_) => {
                        self.errors.push(LowerError::from(integer));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    if let Some(ty) = self.type_of(ident.literal) {
                        Ok(Expr {
                            kind: ExprKind::Ident {
                                literal: ident.literal,
                                ty: ty.kind,
                            },
                            span: ident.span(),
                        })
                    } else {
                        self.errors.push(LowerError::from(ident));
                        Err(PushedErrors)
                    }
                }
                ast::ExprLit::True(tru) => Ok(Expr {
                    kind: ExprKind::Bool(true),
                    span: tru.span(),
                }),
                ast::ExprLit::False(fals) => Ok(Expr {
                    kind: ExprKind::Bool(false),
                    span: fals.span(),
                }),
            },
            ast::Expr::Tuple(tuple) => {
                // Need to prealloc in the arena with defaults (essentially free to
                // construct/destruct) so the inner RefCell doesn't get accessed simultaneously.
                let types = self
                    .hir
                    .list_types
                    .alloc_extend(iter::repeat_with(Type::dummy).take(tuple.len()));
                let exprs = self
                    .hir
                    .list_exprs
                    .alloc_extend(iter::repeat_with(Expr::dummy).take(tuple.len()));

                for (i, ast_expr) in tuple.iter_elements().enumerate() {
                    let hir_expr = self.lower(ast_expr)?;
                    types[i] = hir_expr.ty();
                    exprs[i] = hir_expr;
                }

                Ok(Expr {
                    kind: ExprKind::Tuple {
                        ty: TypeKind::Tuple(types),
                        exprs,
                    },
                    span: tuple.span(),
                })
            }
            ast::Expr::Closure(closure) => {
                let mut inner = self.enter_scope();
                let [lhs, rhs] = inner.pats_of_many_params(closure.head())?;
                let body = inner.lower(closure.head().body)?;
                drop(inner);

                let arm1 = ExprArm {
                    open: closure.head().open,
                    lhs,
                    rhs,
                    close: closure.head().close,
                    body,
                };

                let arms = if let Some(tail) = closure.tail() {
                    let arms = self
                        .hir
                        .list_expr_arms
                        .alloc_extend(iter::repeat_with(ExprArm::dummy).take(1 + tail.len()));

                    for ((_comma, arm), i) in tail.iter().zip(1..) {
                        let mut inner = self.enter_scope();
                        let [lhs, rhs] = inner.pats_of_many_params(arm)?;
                        let body = inner.lower(arm.body)?;
                        drop(inner);

                        self.unify(arm1.lhs.ty(), lhs.ty());
                        self.unify(arm1.rhs.ty(), rhs.ty());
                        self.unify(arm1.body.ty(), body.ty());

                        if self.had_errors() {
                            return Err(PushedErrors);
                        }

                        arms[i] = ExprArm {
                            open: arm.open,
                            lhs,
                            rhs,
                            close: arm.close,
                            body,
                        };
                    }
                    arms[0] = arm1;
                    arms
                } else {
                    std::slice::from_ref(self.hir.list_expr_arms.alloc(arm1))
                };

                Ok(Expr {
                    kind: ExprKind::Closure {
                        ty: TypeKind::Function(self.hir.type_functions.alloc(TypeFunction {
                            lhs: arm1.lhs.ty(),
                            rhs: arm1.rhs.ty(),
                            output: arm1.body.ty(),
                        })),
                        arms,
                    },
                    span: closure.span(),
                })
            }
            ast::Expr::Appl(appl) => {
                let lhs = self.lower(appl.lhs);
                let rhs = self.lower(appl.rhs);
                let function = self.lower(appl.function);

                let (Ok(lhs), Ok(rhs), Ok(function)) = (lhs, rhs, function) else {
                    return Err(PushedErrors);
                };

                let ty = Type {
                    kind: TypeKind::Var(self.hir.new_typevar()),
                    span: appl.span(),
                };

                let expected_function = self.hir.type_functions.alloc(TypeFunction {
                    lhs: lhs.ty(),
                    rhs: rhs.ty(),
                    output: ty,
                });

                self.unify(
                    function.ty(),
                    Type {
                        kind: TypeKind::Function(expected_function),
                        span: appl.function.span(),
                    },
                );

                if self.had_errors() {
                    return Err(PushedErrors);
                }

                Ok(Expr {
                    kind: ExprKind::Appl {
                        ty: ty.kind,
                        appl: self.hir.expr_appls.alloc(ExprAppl { lhs, function, rhs }),
                    },
                    span: appl.span(),
                })
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
                    Ok(int) => Ok(Pat {
                        kind: PatKind::I32(int),
                        span: integer.span(),
                    }),
                    Err(_) => {
                        self.errors.push(LowerError::from(integer));
                        Err(PushedErrors)
                    }
                },
                ast::ExprLit::Ident(ident) => {
                    let ty = TypeKind::Var(self.hir.new_typevar());
                    self.add_local(
                        ident.literal,
                        Type {
                            kind: ty,
                            span: ident.span(),
                        },
                    );
                    Ok(Pat {
                        kind: PatKind::Ident {
                            literal: ident.literal,
                            ty,
                        },
                        span: ident.span(),
                    })
                }
                ast::ExprLit::True(tru) => Ok(Pat {
                    kind: PatKind::Bool(true),
                    span: tru.span(),
                }),
                ast::ExprLit::False(fals) => Ok(Pat {
                    kind: PatKind::Bool(false),
                    span: fals.span(),
                }),
            },
            ast::Pat::Tuple(tuple) => {
                let pats = self
                    .hir
                    .list_pats
                    .alloc_extend(iter::repeat_with(Pat::dummy).take(tuple.len()));
                let types = self
                    .hir
                    .list_types
                    .alloc_extend(iter::repeat_with(Type::dummy).take(tuple.len()));

                for (i, ast_pat) in tuple.iter_elements().enumerate() {
                    let hir_pat = self.lower_pat(ast_pat)?;
                    types[i] = hir_pat.ty();
                    pats[i] = hir_pat;
                }

                Ok(Pat {
                    kind: PatKind::Tuple {
                        ty: TypeKind::Tuple(types),
                        pats,
                    },
                    span: tuple.span(),
                })
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
    fn pats_of_many_params(
        &mut self,
        arm: &ast::ExprArm<'_, 'input>,
    ) -> Result<[Pat<'hir, 'input>; 2], PushedErrors> {
        match &arm.params {
            ast::ExprParams::Zero => Ok([Pat {
                kind: PatKind::unit(),
                span: arm.close.span(),
            }; 2]),
            ast::ExprParams::One(lhs) => {
                let lhs_type = self.lower_param(lhs);
                Ok([
                    lhs_type?,
                    Pat {
                        kind: PatKind::unit(),
                        span: arm.close.span(),
                    },
                ])
            }
            ast::ExprParams::Two(lhs, _, rhs) => {
                let lhs_type = self.lower_param(lhs);
                let rhs_type = self.lower_param(rhs);
                Ok([lhs_type?, rhs_type?])
            }
        }
    }

    /// Unify two types.
    pub fn unify(&mut self, t1: Type<'hir>, t2: Type<'hir>) -> NodeIndex {
        match (t1, t2) {
            (
                Type {
                    kind: TypeKind::I32,
                    ..
                },
                Type {
                    kind: TypeKind::I32,
                    ..
                },
            ) => self.hir.equations.add_rule(Node::Equiv(t1, t2)),
            (
                Type {
                    kind: TypeKind::Bool,
                    ..
                },
                Type {
                    kind: TypeKind::Bool,
                    ..
                },
            ) => self.hir.equations.add_rule(Node::Equiv(t1, t2)),
            (
                Type {
                    kind: TypeKind::Tuple(a),
                    ..
                },
                Type {
                    kind: TypeKind::Tuple(b),
                    ..
                },
            ) => {
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
            (
                Type {
                    kind: TypeKind::Var(var),
                    span: var_span,
                },
                a,
            )
            | (
                a,
                Type {
                    kind: TypeKind::Var(var),
                    span: var_span,
                },
            ) => {
                // Typevar::Bound also tracks where the typevar was bounded,
                // which we can use if we want to. For now, using
                // `.binding()` is easier because it allows us to also
                // get the default if it's unbounded but has a default.
                if let Some(b) = self.hir[var].binding() {
                    let proof = self.unify(a, *b);

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
                } else if self.hir.occurs(var, &a) {
                    self.errors.push(LowerError::CyclicType {
                        var_span,
                        var,
                        ty_span: a.span,
                        ty_kind: a.kind,
                    });
                    self.hir.equations.add_rule(Node::NotEquiv(t1, t2))
                } else {
                    // The actual binding code is here
                    let conclusion = self
                        .hir
                        .equations
                        .add_rule(Node::Binding { var, definition: a });

                    self.hir[var] = Typevar::Bound {
                        ty: a,
                        source: conclusion,
                    };
                    conclusion
                }
            }
            (
                Type {
                    kind: TypeKind::Function(f1),
                    ..
                },
                Type {
                    kind: TypeKind::Function(f2),
                    ..
                },
            ) => {
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
    pub list_expr_arms: &'hir Arena<ExprArm<'hir, 'input>>,
    pub list_exprs: &'hir Arena<Expr<'hir, 'input>>,
    pub list_types: &'hir Arena<Type<'hir>>,
    pub list_pats: &'hir Arena<Pat<'hir, 'input>>,
    pub typevars: Vec<Typevar<'hir>>,
    pub equations: Equations<'hir>,
}

impl<'hir, 'input> Hir<'hir, 'input> {
    pub fn new_typevar(&mut self) -> Var {
        let var = Var(self.typevars.len());
        self.typevars.push(Typevar::Unbound);
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
                let x_type = Type {
                    kind: TypeKind::Var(x),
                    span: dummy,
                };
                let y = self.new_typevar();
                let y_type = Type {
                    kind: TypeKind::Var(y),
                    span: dummy,
                };

                Polytype {
                    typevars: smallvec![x, y],
                    ty: Type {
                        kind: TypeKind::Function(self.type_functions.alloc(TypeFunction {
                            lhs: x_type,
                            rhs: Type {
                                kind: TypeKind::Function(self.type_functions.alloc(TypeFunction {
                                    lhs: x_type,
                                    rhs: Type {
                                        kind: TypeKind::unit(),
                                        span: dummy,
                                    },
                                    output: y_type,
                                })),
                                span: dummy,
                            },
                            output: y_type,
                        })),
                        span: dummy,
                    },
                }
            }),
            ("print", {
                let x = self.new_typevar();
                let x_type = Type {
                    kind: TypeKind::Var(x),
                    span: dummy,
                };

                Polytype {
                    typevars: smallvec![x],
                    ty: Type {
                        kind: TypeKind::Function(self.type_functions.alloc(TypeFunction {
                            lhs: x_type,
                            rhs: Type {
                                kind: TypeKind::unit(),
                                span: dummy,
                            },
                            output: Type {
                                kind: TypeKind::unit(),
                                span: dummy,
                            },
                        })),
                        span: dummy,
                    },
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
            tbl: &HashMap<Var, TypeKind<'hir>>,
            hir: &mut Hir<'hir, '_>,
            ty: Type<'hir>,
        ) -> Type<'hir> {
            match ty.kind {
                TypeKind::Var(var) => Type {
                    kind: tbl[&var],
                    ..ty
                },
                TypeKind::Function(fun) => Type {
                    kind: TypeKind::Function(hir.type_functions.alloc(TypeFunction {
                        lhs: replace_unbound_typevars(tbl, hir, fun.lhs),
                        rhs: replace_unbound_typevars(tbl, hir, fun.rhs),
                        output: replace_unbound_typevars(tbl, hir, fun.output),
                    })),
                    ..ty
                },
                TypeKind::Tuple(types) => {
                    let replaced_types = hir
                        .list_types
                        .alloc_extend(iter::repeat_with(Type::dummy).take(types.len()));
                    for (i, ty) in types.iter().enumerate() {
                        replaced_types[i] = replace_unbound_typevars(tbl, hir, *ty);
                    }

                    Type {
                        kind: TypeKind::Tuple(replaced_types),
                        ..ty
                    }
                }
                _ => ty,
            }
        }

        let tvs_to_replace = polytype
            .typevars
            .iter()
            .map(|tv| (*tv, TypeKind::Var(self.new_typevar())))
            .collect();

        replace_unbound_typevars(&tvs_to_replace, self, polytype.ty)
    }

    /// Convert an [`ast::Type<'_, 'input>`] annotation into an HIR [`Type<'hir>`].
    pub fn type_from_ast(
        &mut self,
        ty: &ast::Type<'_, 'input>,
        map: &HashMap<&str, Type<'hir>>,
    ) -> Type<'hir> {
        match ty {
            ast::Type::Named(named) => match named.name.literal {
                "i32" => Type {
                    kind: TypeKind::I32,
                    span: named.span(),
                },
                "bool" => Type {
                    kind: TypeKind::Bool,
                    span: named.span(),
                },
                other => map.get(other).copied().expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                let types = self
                    .list_types
                    .alloc_extend(iter::repeat_with(Type::dummy).take(tuple.len()));

                for (i, ty) in tuple.iter_elements().enumerate() {
                    types[i] = self.type_from_ast(ty, map);
                }

                Type {
                    kind: TypeKind::Tuple(types),
                    span: tuple.span(),
                }
            }
            ast::Type::Function(fun) => {
                let lhs = self.type_from_ast(fun.lhs, map);
                let rhs = self.type_from_ast(fun.rhs, map);
                let output = self.type_from_ast(fun.ret, map);

                Type {
                    kind: TypeKind::Function(self.type_functions.alloc(TypeFunction {
                        lhs,
                        rhs,
                        output,
                    })),
                    span: fun.span(),
                }
            }
        }
    }

    fn occurs(&self, var: Var, ty: &Type<'_>) -> bool {
        match ty.kind {
            TypeKind::Var(typevar) => {
                if let Some(t) = self[typevar].binding() {
                    self.occurs(var, t)
                } else {
                    var == typevar
                }
            }
            TypeKind::Function(fun) => {
                self.occurs(var, &fun.lhs)
                    || self.occurs(var, &fun.rhs)
                    || self.occurs(var, &fun.output)
            }
            _ => false,
        }
    }

    fn check_equivalence(&self, var: Var, ty: Type<'_>) -> bool {
        if let Type {
            kind: TypeKind::Var(typevar),
            ..
        } = ty
        {
            if let Some(t) = self[typevar].binding() {
                self.check_equivalence(var, *t)
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
