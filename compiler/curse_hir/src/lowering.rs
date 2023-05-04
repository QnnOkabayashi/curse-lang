use crate::{
    Builtin, Edge, Expr, ExprAppl, ExprArm, ExprKind, Hir, LowerError, Node, Pat, PatKind, Ty,
    Type, TypeFunction, TypeKind, TypeTemplate, Typevar,
};
use ast::Span;
use curse_ast as ast;
use petgraph::graph::NodeIndex;
use std::{collections::HashMap, iter};

pub struct Scope<'outer, 'hir, 'input> {
    pub hir: &'outer mut Hir<'hir, 'input>,
    type_map: &'outer HashMap<&'outer str, Type<'hir, 'input>>,
    errors: &'outer mut Vec<LowerError<'hir, 'input>>,
    original_errors_len: usize,
    globals: &'hir HashMap<&'hir str, TypeTemplate<'hir, 'input>>,
    locals: &'outer mut Vec<(&'hir str, Type<'hir, 'input>)>,
    original_locals_len: usize,
}

impl<'outer, 'hir, 'input: 'hir> Scope<'outer, 'hir, 'input> {
    pub fn new(
        hir: &'outer mut Hir<'hir, 'input>,
        type_map: &'outer HashMap<&'outer str, Type<'hir, 'input>>,
        errors: &'outer mut Vec<LowerError<'hir, 'input>>,
        globals: &'hir HashMap<&'hir str, TypeTemplate<'hir, 'input>>,
        locals: &'outer mut Vec<(&'hir str, Type<'hir, 'input>)>,
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
    pub fn type_of(&mut self, var: &str) -> Option<Type<'hir, 'input>> {
        self.locals
            .iter()
            .rev()
            .find_map(|(ident, ty)| (*ident == var).then(|| *ty))
            .or_else(|| {
                self.globals
                    .get(var)
                    .map(|polytype| self.hir.monomorphize(polytype))
            })
    }

    pub fn add_local(&mut self, var: &'hir str, ty: Type<'hir, 'input>) {
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

    // TODO(quinn): Move each branch into its own method to clean up this function.
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
                ast::ExprSymbol::Eq(equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Eq),
                    span: equal.span(),
                }),
                ast::ExprSymbol::Lt(less) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Lt),
                    span: less.span(),
                }),
                ast::ExprSymbol::Gt(greater) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Gt),
                    span: greater.span(),
                }),
                ast::ExprSymbol::Le(less_equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Le),
                    span: less_equal.span(),
                }),
                ast::ExprSymbol::Ge(greater_equal) => Ok(Expr {
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
                    .types
                    .alloc_extend(iter::repeat_with(Type::dummy).take(tuple.len()));
                let exprs = self
                    .hir
                    .exprs
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
            ast::Expr::Closure(closure) => self.lower_closure(closure),
            ast::Expr::Appl(appl) => self.lower_appl(appl),
        }
    }

    /// Lowers an [`ast::ExprClosure`].
    pub fn lower_closure(
        &mut self,
        closure: &ast::ExprClosure<'_, 'input>,
    ) -> Result<Expr<'hir, 'input>, PushedErrors> {
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
                .arms
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
            std::slice::from_ref(self.hir.arms.alloc(arm1))
        };

        Ok(Expr {
            kind: ExprKind::Closure {
                ty: TypeKind::Function(self.hir.type_fns.alloc(TypeFunction {
                    lhs: arm1.lhs.ty(),
                    rhs: arm1.rhs.ty(),
                    output: arm1.body.ty(),
                })),
                arms,
            },
            span: closure.span(),
        })
    }

    /// Lowers an [`ast::ExprAppl`].
    fn lower_appl(
        &mut self,
        appl: &ast::ExprAppl<'_, 'input>,
    ) -> Result<Expr<'hir, 'input>, PushedErrors> {
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

        let expected_function = self.hir.type_fns.alloc(TypeFunction {
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
                appl: self.hir.appls.alloc(ExprAppl { lhs, function, rhs }),
            },
            span: appl.span(),
        })
    }

    /// Returns the [`Type<'hir, 'input>`] of an [`ast::ExprPat`].
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
                    .pats
                    .alloc_extend(iter::repeat_with(Pat::dummy).take(tuple.len()));
                let types = self
                    .hir
                    .types
                    .alloc_extend(iter::repeat_with(Type::dummy).take(tuple.len()));

                for (i, ast_pat) in tuple.iter_elements().enumerate() {
                    let hir_pat = self.lower_pat(ast_pat)?;
                    types[i] = hir_pat.ty();
                    pats[i] = hir_pat;
                }

                Ok(Pat {
                    kind: PatKind::Tuple { ty: types, pats },
                    span: tuple.span(),
                })
            }
            ast::Pat::Choice(_choice) => {
                todo!("lower choice patterns")
            } // When we add struct destructuring, we can unify the type of the field
              // with the returned type of the pattern in that field.
              // e.g. If we have a `struct Number(i32)` and have the pattern
              // `Number(x)`, then `x` might be type `Var(Typevar::Unbound(0))` which
              // we can unify with `i32` to see that it should be `Var(Typevar::Bound(&Int))`
        }
    }

    /// Returns the [`Type<'hir, 'input>`] of a single [`ast::ExprParam`].
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

    /// Returns the [`Type<'hir, 'input>`]s of various [`ast::ExprParams`].
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
    pub fn unify(&mut self, t1: Type<'hir, 'input>, t2: Type<'hir, 'input>) -> NodeIndex {
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
                // TODO(quinn): The problem at hand is we need to be able to unify
                // the types of builtin functions with the types of custom functions.
                // I made an enum `TypeFunctionKind` that allows for either, but its
                // lhs/rhs/output functions return a `&TypeKind`, not a `Type`,
                // meaning I can't unify on it (which was the whole point...).
                // I know that unification only requires the type in the case
                // that we do var binding, and only uses the span if it's a var
                // (which it never will be for builtins), or if it's a cyclic type
                // (which it never is because it's literally addition).
                // So I think I need some other alternative.
                //
                // One idea I had was to create a customized error message when
                // unification fails on a builtin (e.g. "tried to pass bool to `+` operator"),
                // but this wouldn't work well with higher-order functions.
                //
                // A more permanent solution would be to create some pseudo definition
                // site for the builtins and have error messages reference that instead.
                // That would require having miette work on errors with text from different
                // sources though...
                //
                // In pursuit of this idea, I've created a `CORE_ARITH` string at the
                // bottom on `compiler/curse/src/main.rs` that can serve as the function stubs.
                // Now all I need is a way of telling my miette errors which string to use,
                // and then hooking that up in the main fn.
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

#[derive(Debug)]
pub struct PushedErrors;
