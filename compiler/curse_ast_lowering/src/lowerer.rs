use crate::{arena_alloc::ArenaAlloc, LoweringError, UnexpectedTypeArgs};
use curse_arena::new::SliceGuard;
use curse_ast::ast;
use curse_hir::arena::HirArena;
use curse_hir::hir::{
    Appl, Arm, ChoiceDef, Constructor, Expr, ExprKind, ExprRef, FunctionDef, Lit, Map, Param, Pat,
    PatKind, PatRef, Program, StructDef, Symbol, Type, TypeKind, TypeRef,
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

pub trait Lower<'hir> {
    type Lowered;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered;
}

pub struct Lowerer<'hir> {
    arena: &'hir HirArena<'hir>,
    in_scope_generic_params: Option<&'hir [Ident]>,
    pub errors: Vec<LoweringError>,
}

impl<'hir> Lowerer<'hir> {
    pub fn new(arena: &'hir HirArena<'hir>) -> Self {
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

    pub fn alloc<T: ArenaAlloc<'hir>>(&self, value: T) -> &'hir T {
        T::get_arena(self.arena).alloc(value)
    }

    pub fn prealloc_slice<T: ArenaAlloc<'hir>>(&self, capacity: usize) -> SliceGuard<'hir, T> {
        T::get_arena(self.arena).prealloc_slice(capacity)
    }

    fn lower_record<T: LowerToFieldValue, F>(
        &mut self,
        record: &ast::Record<'_, T>,
        lower_field_fn: F,
    ) -> &'hir [(Ident, T::Lowered<'hir>)]
    where
        F: Fn(&ast::Field<'_, T>, Ident, &mut Lowerer<'hir>) -> T::Lowered<'hir>,
        (Ident, T::Lowered<'hir>): ArenaAlloc<'hir>,
    {
        let fields = self
            .prealloc_slice(record.len())
            .extend(record.fields().map(|field| {
                let ident = Ident::from(field.ident);
                (ident, lower_field_fn(field, ident, self))
            }));

        fields.sort_unstable_by(|a, b| a.0.cmp(&b.0));
        fields
    }
}

impl<'hir> Lower<'hir> for ast::Program<'_, '_> {
    type Lowered = Program<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
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
            function_defs: HashMap::with_capacity(self.function_defs.len()),
            struct_defs: HashMap::with_capacity(self.struct_defs.len()),
            choice_defs: HashMap::with_capacity(self.choice_defs.len()),
        };

        for ast_def in self.function_defs.iter() {
            let def = ast_def.lower(lowerer);
            insert_or_push_err(
                program.function_defs.entry(def.ident.string),
                def,
                &mut lowerer.errors,
            );
        }

        for def in self.struct_defs.iter() {
            let def = def.lower(lowerer);
            insert_or_push_err(
                program.struct_defs.entry(def.ident.string),
                def,
                &mut lowerer.errors,
            )
        }

        for def in self.choice_defs.iter() {
            let def = def.lower(lowerer);
            insert_or_push_err(
                program.choice_defs.entry(def.ident.string),
                def,
                &mut lowerer.errors,
            )
        }

        program
    }
}

impl<'hir> Lower<'hir> for ast::StructDef<'_, '_> {
    type Lowered = StructDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let generic_params = self
            .generic_params
            .as_ref()
            .map(|generic_params| generic_params.lower(lowerer))
            .unwrap_or_default();

        let ty = lowerer.with_generic_params(generic_params, |lowerer| self.ty.lower(lowerer));
        let ty = lowerer.alloc(ty);

        StructDef {
            ident: self.ident.into(),
            generic_params,
            ty,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::ChoiceDef<'_, '_> {
    type Lowered = ChoiceDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let generic_params = self
            .generic_params
            .as_ref()
            .map(|generic_params| generic_params.lower(lowerer))
            .unwrap_or_default();

        let variants = match &self.variants {
            ast::Variants::Never(_) => Map::default(),
            ast::Variants::Variants(_, variants, last) => {
                Map::new(lowerer.with_generic_params(generic_params, |lowerer| {
                    lowerer.prealloc_slice(variants.len() + 1).extend(
                        variants
                            .iter()
                            .map(|(variant, _pipe)| variant)
                            .chain(Some(last))
                            .map(|variant| {
                                let ty = variant.ty.lower(lowerer);
                                let ty = lowerer.alloc(ty);
                                (variant.ident.into(), ty)
                            }),
                    )
                }))
            }
        };

        ChoiceDef {
            ident: self.ident.into(),
            generic_params,
            variants,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::GenericParams<'_> {
    type Lowered = &'hir [Ident];

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        match self {
            ast::GenericParams::Single(ident) => {
                slice::from_ref(lowerer.arena.idents.alloc(Ident::from(ident)))
            }
            ast::GenericParams::CartesianProduct(_, ref types, ref last, _) => {
                lowerer.prealloc_slice(types.len() + 1).extend(
                    types
                        .iter()
                        .map(|(ident, _star)| ident)
                        .chain(Some(last))
                        .map(Ident::from),
                )
            }
        }
    }
}

impl<'hir> Lower<'hir> for ast::FunctionDef<'_, '_> {
    type Lowered = FunctionDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let (generic_params, ty) = self
            .explicit_types
            .as_ref()
            .map(|explicit_types| {
                let generic_params = explicit_types
                    .generic_params
                    .as_ref()
                    .map(|x| x.lower(lowerer))
                    .unwrap_or_default();

                let ty = explicit_types.ty.lower(lowerer);
                let ty = Some(&*lowerer.arena.types.alloc(ty));

                (generic_params, ty)
            })
            .unwrap_or_default();

        let function =
            lowerer.with_generic_params(generic_params, |lowerer| self.function.lower(lowerer));

        FunctionDef {
            ident: self.ident.into(),
            generic_params,
            ty,
            function,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::Expr<'_, '_> {
    type Lowered = Expr<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self {
            ast::Expr::Paren(paren) => return paren.expr.lower(lowerer),
            ast::Expr::Symbol(symbol) => ExprKind::Symbol(match symbol {
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
            }),
            ast::Expr::Lit(lit) => match lit.lower(lowerer) {
                Ok(lit) => ExprKind::Lit(lit),
                Err(()) => ExprKind::Error,
            },
            ast::Expr::Record(record) => ExprKind::Record(Map {
                entries: lowerer.lower_record(record, |field, _, lowerer| {
                    field.value.map(|(_colon, expr)| {
                        let expr = expr.lower(lowerer);
                        lowerer.alloc(expr)
                    })
                }),
            }),
            ast::Expr::Constructor(constructor) => {
                ExprKind::Constructor(constructor.lower(lowerer))
            }
            ast::Expr::Closure(closure) => ExprKind::Closure(closure.lower(lowerer)),
            ast::Expr::Appl(appl) => ExprKind::Appl(appl.lower(lowerer)),
            ast::Expr::Error => todo!(),
        };

        Expr {
            span: self.span(),
            kind,
        }
    }
}

impl<'hir> Lower<'hir> for ast::Lit<'_> {
    type Lowered = Result<Lit, ()>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        match self {
            ast::Lit::Integer(integer) => integer.lower(lowerer).map(Lit::Integer),
            ast::Lit::Ident(ident) => Ok(Lit::Ident(Ident::from(ident))),
            ast::Lit::True(_) => Ok(Lit::Bool(true)),
            ast::Lit::False(_) => Ok(Lit::Bool(false)),
        }
    }
}

impl<'hir> Lower<'hir> for ast::tok::Integer<'_> {
    type Lowered = Result<u32, ()>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        // Algorithm: https://github.com/Alexhuszagh/rust-lexical/blob/main/lexical-parse-integer/docs/Algorithm.md
        let mut value: u32 = 0;
        let mut count = 0;
        for byte in self.literal.bytes().filter(|&b| b != b'_') {
            value = value.wrapping_mul(10);
            value = value.wrapping_add((byte - b'0') as u32);
            count += 1;
        }

        let max = 10;
        let min_value = 10u32.pow(max - 1);

        if count > max || (count == max && value < min_value) {
            lowerer.errors.push(LoweringError::IntegerLiteralOverflow {
                literal: self.to_string(),
                span: self.span(),
            });
            Err(())
        } else {
            Ok(value)
        }
    }
}

impl<'hir> Lower<'hir> for ast::Path<'_> {
    type Lowered = &'hir [Ident];

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        lowerer.prealloc_slice(self.parts.len() + 1).extend(
            self.parts
                .iter()
                .map(|(ident, _)| ident.into())
                .chain(Some(self.ident.into())),
        )
    }
}

impl<'hir, T> Lower<'hir> for ast::Constructor<'_, '_, T>
where
    T: Lower<'hir>,
    T::Lowered: ArenaAlloc<'hir> + 'hir,
{
    type Lowered = Constructor<'hir, T::Lowered>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let path = self.path.lower(lowerer);
        let inner = self.inner.lower(lowerer);
        let inner = lowerer.alloc(inner);

        Constructor { path, inner }
    }
}

impl<'hir> Lower<'hir> for ast::Closure<'_, '_> {
    type Lowered = &'hir [Arm<'hir>];

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        match self {
            ast::Closure::NonPiecewise(arm) => {
                let arm = arm.lower(lowerer);
                slice::from_ref(lowerer.alloc(arm))
            }
            ast::Closure::Piecewise(_, arms, last, _) => lowerer
                .prealloc_slice(arms.len() + last.is_some() as usize)
                .extend(
                    arms.iter()
                        .map(|(arm, _comma)| arm)
                        .chain(last.as_ref())
                        .map(|arm| arm.lower(lowerer)),
                ),
            ast::Closure::Empty(_, _) => &[],
        }
    }
}

impl<'hir> Lower<'hir> for ast::Arm<'_, '_> {
    type Lowered = Arm<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let params = lowerer
            .prealloc_slice(self.params.len() + self.last.is_some() as usize)
            .extend(
                self.params
                    .iter()
                    .map(|(arm, _comma)| arm)
                    .chain(self.last.as_ref())
                    .map(|param| param.lower(lowerer)),
            );

        if params.len() > 2 {
            lowerer.errors.push(LoweringError::TooManyClosureParams {
                all_params: params.iter().map(HasSpan::span).collect(),
            });
        }

        let body = self.body.lower(lowerer);
        let body = lowerer.alloc(body);

        Arm { params, body }
    }
}

impl<'hir> Lower<'hir> for ast::Param<'_, '_> {
    type Lowered = Param<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let pat = self.pat.lower(lowerer);
        let pat = lowerer.alloc(pat);

        let ascription = self.ascription.map(|(_colon, ty)| {
            let ty = ty.lower(lowerer);
            lowerer.alloc(ty)
        });

        Param { pat, ascription }
    }
}

impl<'hir> Lower<'hir> for ast::Appl<'_, '_> {
    type Lowered = Appl<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let mut slots = lowerer.prealloc_slice(3);

        slots.push(self.lhs.lower(lowerer));
        slots.push(self.fun.lower(lowerer));
        slots.push(self.rhs.lower(lowerer));

        let parts = (slots.into_slice() as &'hir [Expr<'hir>])
            .try_into()
            .expect("took 3, so slice has to be 3");

        Appl { parts }
    }
}

impl<'hir> Lower<'hir> for ast::Pat<'_, '_> {
    type Lowered = Pat<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self {
            ast::Pat::Lit(lit) => match lit.lower(lowerer) {
                Ok(lit) => PatKind::Lit(lit),
                Err(()) => PatKind::Error,
            },
            ast::Pat::Record(record) => PatKind::Record(Map {
                entries: lowerer.lower_record(record, |field, _, lowerer| {
                    field.value.map(|(_colon, pat)| {
                        let pat = pat.lower(lowerer);
                        &*lowerer.arena.pats.alloc(pat)
                    })
                }),
            }),
            ast::Pat::Constructor(constructor) => {
                let path = constructor.path.lower(lowerer);
                let inner = constructor.inner.lower(lowerer);
                let inner = lowerer.alloc(inner);

                PatKind::Constructor(path, inner)
            }
        };

        Pat {
            kind,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::Type<'_, '_> {
    type Lowered = Type<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self {
            ast::Type::Named(named) => named.lower(lowerer),
            ast::Type::Record(record) => TypeKind::Record(Map {
                entries: lowerer.lower_record(record, |field, field_ident, lowerer| {
                    if let Some((_colon, ty)) = field.value {
                        let ty = ty.lower(lowerer);
                        lowerer.alloc(ty)
                    } else {
                        lowerer
                            .errors
                            .push(LoweringError::TypeRecordMissingFieldType { field_ident });

                        lowerer.alloc(Type {
                            kind: TypeKind::Error,
                            span: field.ident.span(),
                        })
                    }
                }),
            }),
            ast::Type::Error => todo!(),
        };

        Type {
            kind,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::GenericArgs<'_, '_> {
    type Lowered = &'hir [Type<'hir>];

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        match self {
            ast::GenericArgs::Single(ty) => {
                let ty = ty.lower(lowerer);
                slice::from_ref(lowerer.alloc(ty))
            }
            ast::GenericArgs::CartesianProduct(_, types, last, _) => {
                lowerer.prealloc_slice(types.len() + 1).extend(
                    types
                        .iter()
                        .map(|(ty, _star)| ty)
                        .chain(Some(*last))
                        .map(|ty| ty.lower(lowerer)),
                )
            }
        }
    }
}

impl<'hir> Lower<'hir> for ast::NamedType<'_, '_> {
    type Lowered = TypeKind<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let generic_args = self
            .generic_args
            .as_ref()
            .map(|args| args.lower(lowerer))
            .unwrap_or_default();

        let path = self.path.lower(lowerer);

        let [ident] = path else {
            // More than 1 item in the path, can't be a generic or a primitive
            // Note: cannot have 0 elements since `ast::Path` has 1 inlined.
            return TypeKind::Named(path, generic_args);
        };

        // Now that the path and args are lowered, do some light name resolution.

        let err = |reason| LoweringError::UnexpectedTypeArgs {
            reason,
            problem_type_span: self.path.span(),
            arg_spans: generic_args.iter().map(HasSpan::span).collect(),
        };

        let maybe_found_generic = lowerer.in_scope_generic_params.and_then(|generic_params| {
            generic_params
                .iter()
                .copied()
                .enumerate()
                .find(|(_, param)| param.string == ident.string)
        });

        if let Some((def_index, def_ident)) = maybe_found_generic {
            // Name of the type was found in the in-scope generic parameters,
            // so it's a type parameter.
            //
            // Yes, this takes precedence over primitives. You can do
            // `struct Wrap I32 = I32` and the `I32` on the right will refer
            // to the generic type and not the primitive.
            if !generic_args.is_empty() {
                lowerer
                    .errors
                    .push(err(UnexpectedTypeArgs::GenericParam { def_ident }));
            }

            TypeKind::Generic(def_ident, def_index as u32)
        } else if let Ok(prim) = self.path.ident.literal.parse() {
            if !generic_args.is_empty() {
                lowerer
                    .errors
                    .push(err(UnexpectedTypeArgs::Primitive(prim)));
            }

            TypeKind::Primitive(prim)
        } else {
            TypeKind::Named(path, generic_args)
        }
    }
}
