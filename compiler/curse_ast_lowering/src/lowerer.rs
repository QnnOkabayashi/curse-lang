use crate::{Arena, LoweringError, UnexpectedTypeArgs};
use curse_arena::DroplessArena;
use curse_ast as ast;
use curse_hir::{
    Appl, Arm, ChoiceDef, Expr, ExprKind, ExprRef, FunctionDef, Lit, Map, Param, Pat, PatKind,
    PatRef, Program, StructDef, Symbol, Type, TypeKind, TypeRef,
};
use curse_interner::{Ident, InternedString};
use curse_span::HasSpan;
use std::{collections::HashMap, slice};

/// AST nodes that lower to things that can appear as field values.
trait LowerToFieldValue {
    type Lowered<'hir>;
}

impl LowerToFieldValue for &ast::Expr<'_, '_> {
    type Lowered<'hir> = Option<ExprRef<'hir>>;
}

impl LowerToFieldValue for &ast::Pat<'_, '_> {
    type Lowered<'hir> = Option<PatRef<'hir>>;
}

impl LowerToFieldValue for &ast::Type<'_, '_> {
    type Lowered<'hir> = TypeRef<'hir>;
}

pub struct Lowerer<'hir> {
    arena: &'hir Arena<'hir>,
    in_scope_generic_params: Option<&'hir [Ident]>,
    pub errors: Vec<LoweringError>,
}

impl<'hir> Lowerer<'hir> {
    pub fn new(arena: &'hir Arena<'hir>) -> Self {
        Lowerer {
            arena,
            in_scope_generic_params: None,
            errors: Vec::with_capacity(0),
        }
    }

    /// Replace the in-scope generic params for the duration of the closure.
    ///
    /// This is useful because we want to know when a type is some concrete named type, or a
    /// generic type parameter. By keeping track of the generic type parameters of whatever item
    /// we're in, we can do this.
    fn with_generic_params<F, R>(&mut self, generic_params: &'hir [Ident], f: F) -> R
    where
        F: FnOnce(&mut Lowerer<'hir>) -> R,
    {
        let outer_generics = self.in_scope_generic_params.replace(generic_params);
        let res = f(self);
        self.in_scope_generic_params = outer_generics;
        res
    }

    pub fn lower_program(&mut self, ast_program: &ast::Program<'_, '_>) -> Program<'hir> {
        use std::collections::hash_map::Entry;

        /// Inserts the lowered def into the map if the name is unique,
        /// otherwise leaves the original and pushes a `LoweringError`
        /// describing that there are multiple defs with the same name.
        fn insert_or_push_err<T: HasSpan>(
            entry: Entry<'_, InternedString, T>,
            lowered: T,
            errors: &mut Vec<LoweringError>,
        ) {
            match entry {
                Entry::Occupied(occupied) => {
                    errors.push(LoweringError::MultipleDefsWithSameName {
                        ident: *occupied.key(),
                        previous: occupied.get().span(),
                        redefined: lowered.span(),
                    });
                }
                Entry::Vacant(vacant) => {
                    vacant.insert(lowered);
                }
            }
        }

        let mut program = Program {
            function_defs: HashMap::with_capacity(ast_program.function_defs.len()),
            struct_defs: HashMap::with_capacity(ast_program.struct_defs.len()),
            choice_defs: HashMap::with_capacity(ast_program.choice_defs.len()),
        };

        for ast_def in ast_program.function_defs.iter() {
            let def = self.lower_function_def(ast_def);
            insert_or_push_err(
                program.function_defs.entry(def.ident.string),
                def,
                &mut self.errors,
            );
        }

        for ast_def in ast_program.struct_defs.iter() {
            let def = self.lower_struct_def(ast_def);
            insert_or_push_err(
                program.struct_defs.entry(def.ident.string),
                def,
                &mut self.errors,
            );
        }

        for ast_def in ast_program.choice_defs.iter() {
            let def = self.lower_choice_def(ast_def);
            insert_or_push_err(
                program.choice_defs.entry(def.ident.string),
                def,
                &mut self.errors,
            );
        }

        program
    }

    pub fn lower_generic_params(
        &mut self,
        generic_params: Option<&ast::GenericParams<'_>>,
    ) -> &'hir [Ident] {
        match generic_params {
            Some(ast::GenericParams::Single(ident)) => {
                slice::from_ref(self.arena.idents.alloc(Ident::from(ident)))
            }
            Some(ast::GenericParams::CartesianProduct(_, ref types, ref last, _)) => {
                self.arena.idents.alloc_slice(types.len() + 1).extend(
                    types
                        .iter()
                        .map(|(ident, _star)| ident)
                        .chain(Some(last))
                        .map(Ident::from),
                )
            }
            None => &[],
        }
    }

    pub fn lower_function_def(&mut self, def: &ast::FunctionDef<'_, '_>) -> FunctionDef<'hir> {
        let ident = Ident::from(def.ident);

        let (generic_params, ty) = def
            .explicit_types
            .as_ref()
            .map(|explicit_types| {
                let generic_params =
                    self.lower_generic_params(explicit_types.generic_params.as_ref());

                let ty = self.lower_type(explicit_types.ty);
                let ty = Some(&*self.arena.types.alloc(ty));

                (generic_params, ty)
            })
            .unwrap_or_default();

        let function =
            self.with_generic_params(generic_params, |this| this.lower_closure(&def.function));

        FunctionDef {
            ident,
            generic_params,
            ty,
            function,
            span: def.span(),
        }
    }

    pub fn lower_struct_def(&mut self, def: &ast::StructDef<'_, '_>) -> StructDef<'hir> {
        let ident = Ident::from(def.ident);
        let generic_params = self.lower_generic_params(def.generic_params.as_ref());

        let ty = self.with_generic_params(generic_params, |this| this.lower_type(def.inner));
        let ty = &*self.arena.types.alloc(ty);

        StructDef {
            ident,
            generic_params,
            ty,
            span: def.span(),
        }
    }

    pub fn lower_choice_def(&mut self, def: &ast::ChoiceDef<'_, '_>) -> ChoiceDef<'hir> {
        let ident = Ident::from(def.ident);
        let generic_params = self.lower_generic_params(def.generic_params.as_ref());

        let variants = self.with_generic_params(generic_params, |this| match &def.variants {
            ast::Variants::Never(_) => Map { entries: &[] },
            ast::Variants::Variants(_, variants, last) => Map {
                entries: this
                    .arena
                    .variant_defs
                    .alloc_slice(variants.len() + 1)
                    .extend(
                        variants
                            .iter()
                            .map(|(variant_def, _pipe)| variant_def)
                            .chain(Some(last))
                            .map(|variant_def| {
                                let ident = Ident::from(variant_def.ident);
                                let ty = this.lower_type(variant_def.inner);
                                let ty = &*this.arena.types.alloc(ty);
                                (ident, ty)
                            }),
                    ),
            },
        });

        ChoiceDef {
            ident,
            generic_params,
            variants,
            span: def.span(),
        }
    }

    pub fn lower_expr(&mut self, expr: &ast::Expr<'_, '_>) -> Expr<'hir> {
        let kind = match expr {
            ast::Expr::Paren(paren) => return self.lower_expr(paren.expr),
            ast::Expr::Symbol(symbol) => ExprKind::Symbol(Self::lower_symbol(symbol)),
            ast::Expr::Lit(lit) => match self.lower_lit(lit) {
                Ok(lit) => ExprKind::Lit(lit),
                Err(()) => ExprKind::Error,
            },
            ast::Expr::Record(record) => ExprKind::Record(Map {
                entries: self.lower_record(record, &self.arena.expr_fields, |field, _, lowerer| {
                    field.value.map(|(_colon, expr)| {
                        let expr = lowerer.lower_expr(expr);
                        &*lowerer.arena.exprs.alloc(expr)
                    })
                }),
            }),
            ast::Expr::Constructor(constructor) => {
                let (name, inner) = self.lower_constructor(constructor);
                ExprKind::Constructor(name, inner)
            }
            ast::Expr::Closure(closure) => ExprKind::Closure(self.lower_closure(closure)),
            ast::Expr::Appl(appl) => ExprKind::Appl(self.lower_appl(appl)),
            ast::Expr::Error => todo!(),
        };

        Expr {
            span: expr.span(),
            kind,
        }
    }

    fn lower_symbol(symbol: &ast::Symbol) -> Symbol {
        match symbol {
            ast::Symbol::Plus(_) => Symbol::Plus,
            ast::Symbol::Minus(_) => Symbol::Minus,
            ast::Symbol::Star(_) => Symbol::Star,
            ast::Symbol::Dot(_) => Symbol::Dot,
            ast::Symbol::DotDot(_) => Symbol::DotDot,
            ast::Symbol::Semi(_) => Symbol::Semi,
            ast::Symbol::Percent(_) => Symbol::Percent,
            ast::Symbol::Slash(_) => Symbol::Slash,
            ast::Symbol::Eq(_) => Symbol::Eq,
            ast::Symbol::Lt(_) => Symbol::Lt,
            ast::Symbol::Gt(_) => Symbol::Gt,
            ast::Symbol::Le(_) => Symbol::Le,
            ast::Symbol::Ge(_) => Symbol::Ge,
        }
    }

    fn lower_lit(&mut self, lit: &ast::Lit<'_>) -> Result<Lit, ()> {
        match lit {
            ast::Lit::Integer(integer) => self.lower_lit_int(integer).map(Lit::Integer),
            ast::Lit::Ident(ident) => Ok(Lit::Ident(Ident::from(ident))),
            ast::Lit::True(_) => Ok(Lit::Bool(true)),
            ast::Lit::False(_) => Ok(Lit::Bool(false)),
        }
    }

    fn lower_lit_int(&mut self, integer: &ast::tok::Integer<'_>) -> Result<u32, ()> {
        // Algorithm: https://github.com/Alexhuszagh/rust-lexical/blob/main/lexical-parse-integer/docs/Algorithm.md
        let mut value: u32 = 0;
        let mut count = 0;
        for byte in integer.literal.bytes().filter(|&b| b != b'_') {
            value = value.wrapping_mul(10);
            value = value.wrapping_add((byte - b'0') as u32);
            count += 1;
        }

        let max = 10;
        let min_value = 10u32.pow(max - 1);

        if count > max || (count == max && value < min_value) {
            self.errors.push(LoweringError::IntegerLiteralOverflow {
                literal: integer.to_string(),
                span: integer.span(),
            });
            Err(())
        } else {
            Ok(value)
        }
    }

    fn lower_record<T: LowerToFieldValue, F>(
        &mut self,
        record: &ast::Record<'_, T>,
        field_arena: &'hir DroplessArena<(Ident, T::Lowered<'hir>)>,
        lower_field_fn: F,
    ) -> &'hir [(Ident, T::Lowered<'hir>)]
    where
        F: Fn(&ast::Field<'_, T>, Ident, &mut Lowerer<'hir>) -> T::Lowered<'hir>,
    {
        let fields = field_arena
            .alloc_slice(record.fields().count())
            .extend(record.fields().map(|field| {
                let ident = Ident::from(field.ident);
                (ident, lower_field_fn(field, ident, self))
            }));

        fields.sort_unstable_by(|a, b| a.0.cmp(&b.0));
        fields
    }

    fn lower_constructor(
        &mut self,
        constructor: &ast::Constructor<'_, '_>,
    ) -> (Ident, ExprRef<'hir>) {
        let ident = Ident::from(constructor.ident);
        let expr = self.lower_expr(constructor.inner);
        let inner = self.arena.exprs.alloc(expr);

        (ident, inner)
    }

    fn lower_closure(&mut self, closure: &ast::Closure<'_, '_>) -> &'hir [Arm<'hir>] {
        match closure {
            ast::Closure::NonPiecewise(arm) => {
                let arm = self.lower_arm(arm);
                slice::from_ref(self.arena.arms.alloc(arm))
            }
            ast::Closure::Piecewise(_, arms, last, _) => self
                .arena
                .arms
                .alloc_slice(arms.len() + last.is_some() as usize)
                .extend(
                    arms.iter()
                        .map(|(arm, _comma)| arm)
                        .chain(last.as_ref())
                        .map(|arm| self.lower_arm(arm)),
                ),
            ast::Closure::Empty(_, _) => &[],
        }
    }

    fn lower_appl(&mut self, appl: &ast::Appl<'_, '_>) -> Appl<'hir> {
        let mut slots = self.arena.exprs.alloc_slice(3);

        slots.push(self.lower_expr(appl.lhs));
        slots.push(self.lower_expr(appl.fun));
        slots.push(self.lower_expr(appl.rhs));

        let parts = (slots.into_slice() as &'hir [Expr<'hir>])
            .try_into()
            .expect("took 3, so slice has to be 3");

        Appl { parts }
    }

    fn lower_arm(&mut self, arm: &ast::Arm<'_, '_>) -> Arm<'hir> {
        let params = self
            .arena
            .params
            .alloc_slice(arm.params.len() + arm.last.is_some() as usize)
            .extend(
                arm.params
                    .iter()
                    .map(|(arm, _comma)| arm)
                    .chain(arm.last.as_ref())
                    .map(|param| self.lower_param(param)),
            );

        if params.len() > 2 {
            self.errors.push(LoweringError::TooManyClosureParams {
                all_params: params.iter().map(HasSpan::span).collect(),
            });
        }

        let body = self.lower_expr(arm.body);
        let body = self.arena.exprs.alloc(body);

        Arm { params, body }
    }

    fn lower_param(&mut self, param: &ast::Param<'_, '_>) -> Param<'hir> {
        let pat = self.lower_pat(param.pat);
        let pat = self.arena.pats.alloc(pat);

        let ascription = param.ascription.map(|(_colon, ty)| {
            let ty = self.lower_type(ty);
            &*self.arena.types.alloc(ty)
        });

        Param { pat, ascription }
    }

    pub fn lower_pat(&mut self, pat: &ast::Pat<'_, '_>) -> Pat<'hir> {
        let kind = match pat {
            ast::Pat::Lit(lit) => match self.lower_lit(lit) {
                Ok(lit) => PatKind::Lit(lit),
                Err(()) => PatKind::Error,
            },
            ast::Pat::Record(record) => PatKind::Record(Map {
                entries: self.lower_record(record, &self.arena.pat_fields, |field, _, lowerer| {
                    field.value.map(|(_colon, pat)| {
                        let pat = lowerer.lower_pat(pat);
                        &*lowerer.arena.pats.alloc(pat)
                    })
                }),
            }),
            ast::Pat::Struct(ident, inner) => {
                let ident = Ident::from(ident);
                let inner = self.lower_pat(inner);
                let inner = self.arena.pats.alloc(inner);

                PatKind::Struct(ident, inner)
            }
        };

        Pat {
            kind,
            span: pat.span(),
        }
    }

    pub fn lower_type(&mut self, ty: &ast::Type<'_, '_>) -> Type<'hir> {
        let kind =
            match ty {
                ast::Type::Named(named) => self.lower_named_type(named),
                ast::Type::Record(record) => {
                    TypeKind::Record(Map {
                        entries: self.lower_record(
                            record,
                            &self.arena.type_fields,
                            |field, field_ident, lowerer| {
                                if let Some((_colon, ty)) = field.value {
                                    let ty = lowerer.lower_type(ty);
                                    lowerer.arena.types.alloc(ty)
                                } else {
                                    lowerer.errors.push(
                                        LoweringError::TypeRecordMissingFieldType { field_ident },
                                    );

                                    // TODO(quinn): allocating a unit variant feels silly...
                                    &*lowerer.arena.types.alloc(Type {
                                        kind: TypeKind::Error,
                                        span: field.ident.span(),
                                    })
                                }
                            },
                        ),
                    })
                }
                ast::Type::Error => todo!(),
            };

        Type {
            kind,
            span: ty.span(),
        }
    }

    fn lower_generic_args(
        &mut self,
        generic_args: &ast::GenericArgs<'_, '_>,
    ) -> &'hir [Type<'hir>] {
        match generic_args {
            ast::GenericArgs::Single(ty) => {
                let ty = self.lower_type(ty);
                slice::from_ref(self.arena.types.alloc(ty))
            }
            ast::GenericArgs::CartesianProduct(_, types, last, _) => {
                self.arena.types.alloc_slice(types.len() + 1).extend(
                    types
                        .iter()
                        .map(|(ty, _star)| ty)
                        .chain(Some(*last))
                        .map(|ty| self.lower_type(ty)),
                )
            }
        }
    }

    fn lower_named_type(&mut self, named: &ast::NamedType<'_, '_>) -> TypeKind<'hir> {
        let generic_args = named
            .generic_args
            .as_ref()
            .map(|args| self.lower_generic_args(args))
            .unwrap_or_default();

        let ident = Ident::from(named.ident);

        // Now that the ident and args are lowered, do some light name resolution.

        let err = |reason| LoweringError::UnexpectedTypeArgs {
            reason,
            problem_type_span: named.ident.span(),
            arg_spans: generic_args.iter().map(HasSpan::span).collect(),
        };

        let maybe_found_generic = self.in_scope_generic_params.and_then(|generic_params| {
            (0..).zip(generic_params).find_map(|(index, param)| {
                (param.string == ident.string).then_some((index, *param))
            })
        });

        if let Some((def_index, def_ident)) = maybe_found_generic {
            // Name of the type was found in the in-scope generic parameters,
            // so it's a type parameter.
            //
            // Yes, this takes precedence over primitives. You can do
            // `struct Wrap I32 = I32` and the `I32` on the right will refer
            // to the generic type and not the primitive.
            if !generic_args.is_empty() {
                self.errors
                    .push(err(UnexpectedTypeArgs::GenericParam { def_ident }));
            }

            TypeKind::Generic(ident, def_index)
        } else if let Ok(prim) = named.ident.literal.parse() {
            if !generic_args.is_empty() {
                self.errors.push(err(UnexpectedTypeArgs::Primitive(prim)));
            }

            TypeKind::Primitive(prim)
        } else {
            TypeKind::Named(ident, generic_args)
        }
    }
}
