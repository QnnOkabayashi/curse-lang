use crate::{
    ctx, Builtin, Edge, Expr, ExprAppl, ExprArm, ExprFields, ExprKind, LowerError, Node, Pat,
    PatKind, Ty, Type, TypeFunction, TypeKind, TypeSymbol, TypeTemplate, Typevar, ValueIdent,
    ValueSymbol,
};
use ast::Span;
use curse_ast as ast;
use petgraph::graph::NodeIndex;
use std::{collections::HashMap, iter};

pub struct Scope<'outer, 'cx> {
    pub ctx: &'outer mut ctx::Typeck<'cx>,
    type_map: &'outer HashMap<TypeSymbol, Type<'cx>>,
    errors: &'outer mut Vec<LowerError<'cx>>,
    original_errors_len: usize,
    globals: &'cx HashMap<ValueSymbol, TypeTemplate<'cx>>,
    locals: &'outer mut Vec<(ValueIdent, Type<'cx>)>,
    original_locals_len: usize,
}

impl<'outer, 'cx: 'outer> Scope<'outer, 'cx> {
    pub fn new(
        ctx: &'outer mut ctx::Typeck<'cx>,
        type_map: &'outer HashMap<TypeSymbol, Type<'cx>>,
        errors: &'outer mut Vec<LowerError<'cx>>,
        globals: &'cx HashMap<ValueSymbol, TypeTemplate<'cx>>,
        locals: &'outer mut Vec<(ValueIdent, Type<'cx>)>,
    ) -> Self {
        let original_errors_len = errors.len();
        let original_locals_len = locals.len();
        Scope {
            ctx,
            type_map,
            errors,
            original_errors_len,
            globals,
            locals,
            original_locals_len,
        }
    }

    /// Search through local variables first, then search through global variables.
    pub fn type_of(&mut self, var: ValueIdent) -> Option<Type<'cx>> {
        self.locals
            .iter()
            .rev()
            .find_map(|(ident, ty)| (ident.symbol == var.symbol).then(|| *ty))
            .or_else(move || {
                self.globals
                    .get(&var.symbol)
                    .map(|polytype| self.ctx.monomorphize(polytype))
            })
    }

    pub fn add_local(&mut self, var: ValueIdent, ty: Type<'cx>) {
        self.locals.push((var, ty));
    }

    /// Enter a new scope.
    ///
    /// This method will uniquely borrow a `Scope` to create another `Scope`,
    /// which represents an inner scope. When the returned type is dropped,
    /// all bindings that were added in the inner scope will be removed,
    /// leaving the original scope in its initial state and accessible again
    /// since it's no longer borrowed.
    pub fn enter_scope(&mut self) -> Scope<'_, 'cx> {
        Scope::new(
            self.ctx,
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
    pub fn lower(&mut self, expr: &ast::Expr<'_, '_>) -> Result<Expr<'cx>, PushedErrors> {
        match expr {
            ast::Expr::Paren(paren) => self.lower(paren.expr),
            ast::Expr::Symbol(symbol) => match symbol {
                ast::Symbol::Plus(plus) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Add),
                    span: plus.span(),
                }),
                ast::Symbol::Minus(minus) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Sub),
                    span: minus.span(),
                }),
                ast::Symbol::Star(star) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Mul),
                    span: star.span(),
                }),
                ast::Symbol::Percent(percent) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Rem),
                    span: percent.span(),
                }),
                ast::Symbol::Slash(slash) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Div),
                    span: slash.span(),
                }),
                ast::Symbol::Dot(_dot) => todo!("lower `.`"),
                ast::Symbol::DotDot(_dotdot) => todo!("lower `..`"),
                ast::Symbol::Semi(_semi) => todo!("lower `;`"),
                ast::Symbol::Eq(equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Eq),
                    span: equal.span(),
                }),
                ast::Symbol::Lt(less) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Lt),
                    span: less.span(),
                }),
                ast::Symbol::Gt(greater) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Gt),
                    span: greater.span(),
                }),
                ast::Symbol::Le(less_equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Le),
                    span: less_equal.span(),
                }),
                ast::Symbol::Ge(greater_equal) => Ok(Expr {
                    kind: ExprKind::Builtin(Builtin::Ge),
                    span: greater_equal.span(),
                }),
            },
            ast::Expr::Lit(lit) => match lit {
                ast::Lit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Expr {
                        kind: ExprKind::I32(int),
                        span: integer.span(),
                    }),
                    Err(_) => {
                        self.errors.push(LowerError::from(integer));
                        Err(PushedErrors)
                    }
                },
                ast::Lit::Ident(ident) => {
                    let interned = self.ctx.global.get_or_intern_value_ident(ident);
                    if let Some(ty) = self.type_of(interned) {
                        Ok(Expr {
                            kind: ExprKind::Ident {
                                literal: interned,
                                ty: ty.kind,
                            },
                            span: ident.span(),
                        })
                    } else {
                        self.errors.push(LowerError::from(ident));
                        Err(PushedErrors)
                    }
                }
                ast::Lit::True(tru) => Ok(Expr {
                    kind: ExprKind::Bool(true),
                    span: tru.span(),
                }),
                ast::Lit::False(fals) => Ok(Expr {
                    kind: ExprKind::Bool(false),
                    span: fals.span(),
                }),
            },
            ast::Expr::Record(record) => {
                let (types, exprs) = tuple
                    .elements
                    .as_ref()
                    .map(|nonempty| {
                        let len = 1
                            + nonempty.remaining.len()
                            + if nonempty.trailing.is_some() { 1 } else { 0 };

                        let types = self
                            .ctx
                            .global
                            .types
                            .alloc_extend(iter::repeat_with(Type::default).take(len));

                        let exprs = self
                            .ctx
                            .global
                            .exprs
                            .alloc_extend(iter::repeat_with(Expr::default).take(len));

                        iter::once(nonempty.first)
                            .chain(nonempty.remaining.iter().map(|(expr, _)| expr))
                            .chain(nonempty.trailing)
                            .enumerate()
                            .try_for_each(|(index, expr)| {
                                self.lower(expr).map(|expr| {
                                    types[index] = expr.ty();
                                    exprs[index] = expr;
                                })
                            })
                            .map(|()| (types, exprs))
                    })
                    .transpose()?
                    .unwrap_or_default();

                Ok(Expr {
                    kind: ExprKind::Record {
                        ty: TypeKind::Record(types),
                        exprs,
                    },
                    span: tuple.span(),
                })
            }
            ast::Expr::Constructor(constructor) => {
                todo!()
            },
            ast::Expr::Closure(closure) => self.lower_closure(closure),
            ast::Expr::Appl(appl) => self.lower_appl(appl),
            ast::Expr::Error => todo!("error handling :)"),
        }
    }

    pub fn lower_constructor(
        &mut self,
        constructor: &ast::Constructor<'_, '_>,
    ) -> Result<Expr<'cx>, PushedErrors> {
        // TODO(quinn): figure out what the type is based on the
        // name by searching for it in some ctx

        let fields = match constructor.fields {
            ast::fields::FieldsKind::Newtype(expr) => {
                let lowered = self.ctx.global.exprs.alloc(self.lower(expr)?);
                ExprFields::Newtype(lowered)
            }
            ast::fields::FieldsKind::Record { ref fields, .. } => {
                let lowered = self
                    .ctx
                    .global
                    .exprs
                    .alloc_extend(iter::repeat_with(Expr::default).take(fields.len()));

                for (slot, (field, _comma)) in lowered.iter_mut().zip(fields) {
                    *slot = self.lower(&field.value)?;
                }

                ExprFields::Record(lowered)
            }
        };

        Ok(Expr {
            kind: ExprKind::Constructor {
                fields,
                ty: todo!("figure out the type of the ctor"),
            },
            span: constructor.span(),
        })
    }

    /// Lowers an [`astClosure`].
    pub fn lower_closure(
        &mut self,
        closure: &ast::Closure<'_, '_>,
    ) -> Result<Expr<'cx>, PushedErrors> {
        // Preallocate so we can arena allocate recursively
        let arms = self
            .ctx
            .global
            .arms
            .alloc_extend(iter::repeat_with(ExprArm::default).take(closure.arm_count()));

        let mut unifying_types: Option<[Type; 3]> = None;

        for (slot, arm) in arms.iter_mut().zip(closure.iter_arms()) {
            let mut inner = self.enter_scope();
            let [lhs, rhs] = inner.pats_of_many_params(arm)?;
            let body = inner.lower(arm.body)?;
            drop(inner);

            if let Some([lhs1, rhs1, body1]) = unifying_types {
                self.unify(lhs1, lhs.ty());
                self.unify(rhs1, rhs.ty());
                self.unify(body1, body.ty());

                if self.had_errors() {
                    return Err(PushedErrors);
                }
            } else {
                unifying_types = Some([lhs.ty(), rhs.ty(), body.ty()]);
            }

            *slot = ExprArm {
                open: arm.open,
                lhs,
                rhs,
                close: arm.close,
                body,
            };
        }

        let [lhs, rhs, body] = unifying_types.expect("at least one closure arm");

        Ok(Expr {
            kind: ExprKind::Closure {
                ty: TypeKind::Function(self.ctx.global.type_fns.alloc(TypeFunction {
                    lhs,
                    rhs,
                    output: body,
                })),
                arms,
            },
            span: closure.span(),
        })
    }

    /// Lowers an [`ast::Appl`].
    fn lower_appl(&mut self, appl: &ast::Appl<'_, '_>) -> Result<Expr<'cx>, PushedErrors> {
        let lhs = self.lower(appl.lhs);
        let rhs = self.lower(appl.rhs);
        let function = self.lower(appl.fun);

        let (Ok(lhs), Ok(rhs), Ok(function)) = (lhs, rhs, function) else {
            return Err(PushedErrors);
        };

        let ty = Type {
            kind: TypeKind::Var(self.ctx.new_typevar()),
            span: appl.span(),
        };

        let expected_function = self.ctx.global.type_fns.alloc(TypeFunction {
            lhs: lhs.ty(),
            rhs: rhs.ty(),
            output: ty,
        });

        self.unify(
            function.ty(),
            Type {
                kind: TypeKind::Function(expected_function),
                span: appl.fun.span(),
            },
        );

        if self.had_errors() {
            return Err(PushedErrors);
        }

        Ok(Expr {
            kind: ExprKind::Appl {
                ty: ty.kind,
                appl: self.ctx.global.appls.alloc(ExprAppl { lhs, function, rhs }),
            },
            span: appl.span(),
        })
    }

    /// Returns the [`Type<'cx>`] of an [`astPat`].
    fn lower_pat(&mut self, pat: &ast::Pat<'_, '_>) -> Result<Pat<'cx>, PushedErrors> {
        match pat {
            ast::Pat::Lit(lit) => match lit {
                ast::Lit::Integer(integer) => match integer.literal.parse() {
                    Ok(int) => Ok(Pat {
                        kind: PatKind::I32(int),
                        span: integer.span(),
                    }),
                    Err(_) => {
                        self.errors.push(LowerError::from(integer));
                        Err(PushedErrors)
                    }
                },
                ast::Lit::Ident(ident) => {
                    let ty = TypeKind::Var(self.ctx.new_typevar());
                    self.add_local(
                        self.ctx.global.get_or_intern_value_ident(ident),
                        Type {
                            kind: ty,
                            span: ident.span(),
                        },
                    );
                    Ok(Pat {
                        kind: PatKind::Ident {
                            literal: self.ctx.global.get_or_intern_value_ident(ident),
                            ty,
                        },
                        span: ident.span(),
                    })
                }
                ast::Lit::True(tru) => Ok(Pat {
                    kind: PatKind::Bool(true),
                    span: tru.span(),
                }),
                ast::Lit::False(fals) => Ok(Pat {
                    kind: PatKind::Bool(false),
                    span: fals.span(),
                }),
            },
            ast::Pat::Tuple(tuple) => {
                let pats = self
                    .ctx
                    .global
                    .pats
                    .alloc_extend(iter::repeat_with(Pat::default).take(tuple.len()));

                let types = self
                    .ctx
                    .global
                    .types
                    .alloc_extend(iter::repeat_with(Type::default).take(tuple.len()));

                for (i, ast_pat) in tuple.iter_elements().enumerate() {
                    let hir_pat = self.lower_pat(ast_pat)?;
                    types[i] = hir_pat.ty();
                    pats[i] = hir_pat;
                }

                Ok(Pat {
                    kind: PatKind::Tuple { ty: types, pats },
                    span: tuple.span(),
                })
            } // When we add struct destructuring, we can unify the type of the field
              // with the returned type of the pattern in that field.
              // e.g. If we have a `struct Number(i32)` and have the pattern
              // `Number(x)`, then `x` might be type `Var(Typevar::Unbound(0))` which
              // we can unify with `i32` to see that it should be `Var(Typevar::Bound(&Int))`
        }
    }

    /// Returns the [`Type<'cx>`] of a single [`astParam`].
    fn lower_param(&mut self, param: &ast::Param<'_, '_>) -> Result<Pat<'cx>, PushedErrors> {
        let pat = self.lower_pat(param.pat)?;
        if let Some((_, annotation)) = param.ascription {
            let t2 = self.ctx.type_from_ast(annotation, self.type_map);
            self.unify(pat.ty(), t2);
            if self.had_errors() {
                return Err(PushedErrors);
            }
        }

        Ok(pat)
    }

    /// Returns the [`Type<'cx>`]s of various [`astParams`].
    fn pats_of_many_params(
        &mut self,
        arm: &ast::Arm<'_, '_>,
    ) -> Result<[Pat<'cx>; 2], PushedErrors> {
        let mut params = arm.params.iter().map(|(param, _comma)| param);

        let mut pats = [Pat {
            kind: PatKind::unit(),
            span: arm.close.span(),
        }; 2];

        for (pat, param) in pats.iter_mut().zip(params.by_ref()) {
            *pat = self.lower_param(param)?;
        }

        if params.count() == 0 {
            Ok(pats)
        } else {
            self.errors.push(LowerError::TooManyParams {
                span: (arm.open.start(), arm.close.end()).into(),
            });
            Err(PushedErrors)
        }
    }

    /// Unify two types.
    pub fn unify(&mut self, t1: Type<'cx>, t2: Type<'cx>) -> NodeIndex {
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
            ) => self.ctx.equations.add_rule(Node::Equiv(t1, t2)),
            (
                Type {
                    kind: TypeKind::Bool,
                    ..
                },
                Type {
                    kind: TypeKind::Bool,
                    ..
                },
            ) => self.ctx.equations.add_rule(Node::Equiv(t1, t2)),
            (
                Type {
                    kind: TypeKind::Record(a),
                    ..
                },
                Type {
                    kind: TypeKind::Record(b),
                    ..
                },
            ) => {
                if a.len() == b.len() {
                    let conclusion = self.ctx.equations.add_rule(Node::Equiv(t1, t2));

                    for (i, (&t1, &t2)) in a.iter().zip(b.iter()).enumerate() {
                        let mut inner = self.enter_scope();
                        let proof = inner.unify(t1, t2);
                        if inner.had_errors() {
                            inner.ctx.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                        }
                        inner
                            .ctx
                            .equations
                            .add_proof(proof, conclusion, Edge::Tuple(i));
                    }
                    conclusion
                } else {
                    // different length tuples
                    self.errors.push(LowerError::unify(t1, t2, self.ctx));
                    self.ctx.equations.add_rule(Node::NotEquiv(t1, t2))
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
                if let Some(b) = self.ctx[var].binding() {
                    let proof = self.unify(a, *b);

                    let conclusion = if self.had_errors() {
                        self.ctx.equations.add_rule(Node::NotEquiv(t1, t2))
                    } else {
                        self.ctx.equations.add_rule(Node::Equiv(t1, t2))
                    };
                    // If we wanted, we could also add an edge with `_binding_source`,
                    // which tells us exactly where the typevar was bound.
                    self.ctx
                        .equations
                        .add_proof(proof, conclusion, Edge::Transitivity);
                    conclusion
                } else if self.ctx.check_equivalence(var, a) {
                    self.ctx.equations.add_rule(Node::Equiv(t1, t2))
                } else if self.ctx.occurs(var, &a) {
                    self.errors.push(LowerError::CyclicType {
                        var_span,
                        var,
                        ty_span: a.span,
                        ty_kind: a.kind,
                    });
                    self.ctx.equations.add_rule(Node::NotEquiv(t1, t2))
                } else {
                    // The actual binding code is here
                    let conclusion = self
                        .ctx
                        .equations
                        .add_rule(Node::Binding { var, definition: a });

                    self.ctx[var] = Typevar::Bound { ty: a };
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
                let conclusion = self.ctx.equations.add_rule(Node::Equiv(t1, t2));
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
                let lhs_proof = self.unify(f1.lhs, f2.lhs);
                let rhs_proof = self.unify(f1.rhs, f2.rhs);
                let output_proof = self.unify(f1.output, f2.output); // here

                if self.had_errors() {
                    self.ctx.equations.graph[conclusion] = Node::NotEquiv(t1, t2);
                }

                self.ctx
                    .equations
                    .add_proof(lhs_proof, conclusion, Edge::FunctionLhs);
                self.ctx
                    .equations
                    .add_proof(rhs_proof, conclusion, Edge::FunctionRhs);
                self.ctx
                    .equations
                    .add_proof(output_proof, conclusion, Edge::FunctionOutput);

                conclusion
            }
            _ => {
                self.errors.push(LowerError::unify(t1, t2, self.ctx));
                self.ctx.equations.add_rule(Node::NotEquiv(t1, t2))
            }
        }
    }
}

impl Drop for Scope<'_, '_> {
    fn drop(&mut self) {
        self.locals.truncate(self.original_locals_len);
    }
}

#[derive(Debug)]
pub struct PushedErrors;
