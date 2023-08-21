use crate::error::RegionError;
use crate::{LoweringError, UnexpectedTypeArgs};
use bumpalo::Bump;
use curse_ast::ast;
use curse_hir::hir::{
    Appl, Arm, ChoiceDef, Constructor, Expr, ExprKind, ExprRef, FunctionDef, Lit, Map, Param, Pat,
    PatKind, PatRef, Program, Region, RegionKind, StructDef, Symbol, Type, TypeKind, TypeRef,
};
use curse_interner::{Ident, InternedString};
use curse_span::HasSpan;
use std::{collections::HashMap, slice};

/// AST nodes that lower to things that can appear as field values.
trait LowerToFieldValue {
    type Lowered<'hir>;
}

impl LowerToFieldValue for &ast::Expr<'_> {
    type Lowered<'hir> = Option<ExprRef<'hir>>;
}

impl LowerToFieldValue for &ast::Pat<'_> {
    type Lowered<'hir> = Option<PatRef<'hir>>;
}

impl LowerToFieldValue for &ast::Type<'_> {
    type Lowered<'hir> = TypeRef<'hir>;
}

pub trait Lower<'hir> {
    type Lowered;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered;
}

pub struct Lowerer<'hir> {
    pub bump: &'hir Bump,
    in_scope_generic_params: Option<&'hir [Ident]>,
    pub errors: Vec<LoweringError>,
}

impl<'hir> Lowerer<'hir> {
    pub fn new(bump: &'hir Bump) -> Self {
        Lowerer {
            bump,
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

    fn lower_record<T: LowerToFieldValue, F>(
        &mut self,
        record: &ast::Record<T>,
        lower_field_fn: F,
    ) -> &'hir [(Ident, T::Lowered<'hir>)]
    where
        F: Fn(&ast::Field<T>, Ident, &mut Lowerer<'hir>) -> T::Lowered<'hir>,
    {
        let fields = self
            .bump
            .alloc_slice_fill_iter(record.iter_fields().map(|field| {
                let ident = Ident::from(field.ident);
                (ident, lower_field_fn(field, ident, self))
            }));

        fields.sort_unstable_by(|a, b| a.0.cmp(&b.0));
        fields
    }
}

impl<'hir> Lower<'hir> for ast::Program<'_> {
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
                program.function_defs.entry(def.ident.symbol),
                def,
                &mut lowerer.errors,
            );
        }

        for def in self.struct_defs.iter() {
            let def = def.lower(lowerer);
            insert_or_push_err(
                program.struct_defs.entry(def.ident.symbol),
                def,
                &mut lowerer.errors,
            )
        }

        for def in self.choice_defs.iter() {
            let def = def.lower(lowerer);
            insert_or_push_err(
                program.choice_defs.entry(def.ident.symbol),
                def,
                &mut lowerer.errors,
            )
        }

        program
    }
}

impl<'hir> Lower<'hir> for ast::StructDef<'_> {
    type Lowered = StructDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let generic_params = self
            .generic_params
            .as_ref()
            .map(|generic_params| generic_params.lower(lowerer))
            .unwrap_or_default();

        let ty = lowerer.with_generic_params(generic_params, |lowerer| self.ty.lower(lowerer));
        let ty = lowerer.bump.alloc(ty);

        StructDef {
            ident: self.ident.into(),
            generic_params,
            ty,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::ChoiceDef<'_> {
    type Lowered = ChoiceDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let generic_params = self
            .generic_params
            .as_ref()
            .map(|generic_params| generic_params.lower(lowerer))
            .unwrap_or_default();

        let variants = lowerer.with_generic_params(generic_params, |lowerer| {
            Map::new(
                lowerer
                    .bump
                    .alloc_slice_fill_iter(self.variants.iter_variants().map(|variant| {
                        let ty = variant.ty.lower(lowerer);
                        let ty = &*lowerer.bump.alloc(ty);
                        (variant.ident.into(), ty)
                    })),
            )
        });

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
        lowerer
            .bump
            .alloc_slice_fill_iter(self.iter_params().map(Ident::from))
    }
}

impl<'hir> Lower<'hir> for ast::FunctionDef<'_> {
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
                let ty = Some(&*lowerer.bump.alloc(ty));

                (generic_params, ty)
            })
            .unwrap_or_default();

        let arms =
            lowerer.with_generic_params(generic_params, |lowerer| self.function.lower(lowerer));

        FunctionDef {
            ident: Ident::from(self.ident),
            generic_params,
            ty,
            arms,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::Expr<'_> {
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
                        &*lowerer.bump.alloc(expr)
                    })
                }),
            }),
            ast::Expr::Constructor(constructor) => {
                ExprKind::Constructor(constructor.lower(lowerer))
            }
            ast::Expr::Closure(closure) => ExprKind::Closure(closure.lower(lowerer)),
            ast::Expr::Appl(appl) => ExprKind::Appl(appl.lower(lowerer)),
            ast::Expr::Region(region) => ExprKind::Region(region.lower(lowerer)),
            ast::Expr::Field(_expr, _field) => todo!("lowering fields"),
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
            ast::Lit::Integer(integer) => {
                parse_u32(integer.literal).map(Lit::Integer).ok_or_else(|| {
                    lowerer.errors.push(LoweringError::IntegerLiteralOverflow {
                        literal: integer.to_string(),
                        span: integer.span(),
                    });
                })
            }
            ast::Lit::Ident(ident) => Ok(Lit::Ident(Ident::from(ident))),
            ast::Lit::True(_) => Ok(Lit::Bool(true)),
            ast::Lit::False(_) => Ok(Lit::Bool(false)),
        }
    }
}

/// Assumes `s` is made of ascii digits and `_`. Any other bytes will produce
/// garbage results, but this method is safe since it's written in entirely
/// safe code.
fn parse_u32(s: &str) -> Option<u32> {
    // Algorithm: https://github.com/Alexhuszagh/rust-lexical/blob/main/lexical-parse-integer/docs/Algorithm.md
    let mut value: u32 = 0;
    let mut count = 0;
    for byte in s.bytes().filter(|&b| b != b'_') {
        value = value.wrapping_mul(10);
        value = value.wrapping_add((byte - b'0') as u32);
        count += 1;
    }

    let max_digits_in_u32 = 10;
    let min_value_with_max_digits = 10u32.pow(max_digits_in_u32 - 1);

    // check for EITHER
    // 1. too many digits
    // 2. okay number of digits but overflowed
    if count > max_digits_in_u32
        || (count == max_digits_in_u32 && value < min_value_with_max_digits)
    {
        None
    } else {
        Some(value)
    }
}

impl<'hir> Lower<'hir> for ast::Path<'_> {
    type Lowered = &'hir [Ident];

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        lowerer
            .bump
            .alloc_slice_fill_iter(self.iter_parts().map(Ident::from))
    }
}

impl<'hir, T> Lower<'hir> for ast::Constructor<'_, T>
where
    T: Lower<'hir>,
    T::Lowered: 'hir,
{
    type Lowered = Constructor<'hir, T::Lowered>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let path = self.path.lower(lowerer);
        let inner = self.inner.lower(lowerer);
        let inner = lowerer.bump.alloc(inner);

        Constructor { path, inner }
    }
}

impl<'hir> Lower<'hir> for ast::Closure<'_> {
    type Lowered = &'hir [Arm<'hir>];

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        lowerer
            .bump
            .alloc_slice_fill_iter(self.iter_arms().map(|arm| arm.lower(lowerer)))
    }
}

impl<'hir> Lower<'hir> for ast::Arm<'_> {
    type Lowered = Arm<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let params = lowerer
            .bump
            .alloc_slice_fill_iter(self.iter_params().map(|param| param.lower(lowerer)));

        if params.len() > 2 {
            lowerer.errors.push(LoweringError::TooManyClosureParams {
                all_params: params.iter().map(HasSpan::span).collect(),
            });
        }

        let body = self.body.lower(lowerer);
        let body = lowerer.bump.alloc(body);

        Arm { params, body }
    }
}

impl<'hir> Lower<'hir> for ast::Param<'_> {
    type Lowered = Param<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let pat = self.pat.lower(lowerer);
        let pat = lowerer.bump.alloc(pat);

        let ascription = self.ascription.map(|(_colon, ty)| {
            let ty = ty.lower(lowerer);
            &*lowerer.bump.alloc(ty)
        });

        Param { pat, ascription }
    }
}

impl<'hir> Lower<'hir> for ast::Appl<'_> {
    type Lowered = Appl<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let parts: &[Expr<'hir>] = lowerer.bump.alloc_slice_fill_iter(
            [self.lhs, self.fun, self.rhs]
                .into_iter()
                .map(|expr| expr.lower(lowerer)),
        );

        Appl {
            parts: parts.try_into().expect("slice was made with 3 elements"),
        }
    }
}

impl<'hir> Lower<'hir> for ast::Region<'_> {
    type Lowered = Region<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self.kind {
            ast::RegionKind::Ref(_) => RegionKind::Ref,
            ast::RegionKind::Mut(_) => RegionKind::Mut,
            ast::RegionKind::RefMut(_, _) => RegionKind::RefMut,
        };

        let body = self.body.lower(lowerer);
        let body = lowerer.bump.alloc(body);

        let shadows = match self.pat {
            ast::Pat::Lit(lit) => match lit {
                ast::Lit::Ident(ident) => {
                    Ok(slice::from_ref(lowerer.bump.alloc(Ident::from(ident))))
                }
                ast::Lit::Integer(int) => Err((RegionError::LiteralNumber, int.span())),
                ast::Lit::True(tru) => Err((RegionError::LiteralTrue, tru.span())),
                ast::Lit::False(fals) => Err((RegionError::LiteralFalse, fals.span())),
            }
            .unwrap_or_else(|(err, span)| {
                lowerer.errors.push(LoweringError::Region(err, span));
                &[]
            }),
            ast::Pat::Record(record) => {
                lowerer
                    .bump
                    .alloc_slice_fill_iter(record.iter_fields().map(|field| {
                        if let Some((_colon, pat)) = field.value {
                            lowerer.errors.push(LoweringError::Region(
                                RegionError::RecordWithValue,
                                pat.span(),
                            ));
                        }
                        Ident::from(field.ident)
                    }))
            }
            ast::Pat::Constructor(_) => todo!("constructors in regions are currently unsupported"),
        };

        Region {
            kind,
            shadows,
            body,
        }
    }
}

impl<'hir> Lower<'hir> for ast::Pat<'_> {
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
                        &*lowerer.bump.alloc(pat)
                    })
                }),
            }),
            ast::Pat::Constructor(constructor) => {
                let path = constructor.path.lower(lowerer);
                let inner = constructor.inner.lower(lowerer);
                let inner = lowerer.bump.alloc(inner);

                PatKind::Constructor(path, inner)
            }
        };

        Pat {
            kind,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::Type<'_> {
    type Lowered = Type<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self {
            ast::Type::Named(named) => named.lower(lowerer),
            ast::Type::Record(record) => TypeKind::Record(Map {
                entries: lowerer.lower_record(record, |field, field_ident, lowerer| {
                    if let Some((_colon, ty)) = field.value {
                        let ty = ty.lower(lowerer);
                        lowerer.bump.alloc(ty)
                    } else {
                        lowerer
                            .errors
                            .push(LoweringError::TypeRecordMissingFieldType { field_ident });

                        lowerer.bump.alloc(Type {
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

impl<'hir> Lower<'hir> for ast::GenericArgs<'_> {
    type Lowered = &'hir [Type<'hir>];

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        lowerer
            .bump
            .alloc_slice_fill_iter(self.iter_args().map(|ty| ty.lower(lowerer)))
    }
}

impl<'hir> Lower<'hir> for ast::NamedType<'_> {
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
            // Note: path cannot have 0 elements since `ast::Path` has 1 inlined.
            return TypeKind::Named(path, generic_args);
        };

        // Now that the path and args are lowered, do some light name resolution.
        let err = |reason| LoweringError::UnexpectedTypeArgs {
            reason,
            problem_type_span: self.path.span(),
            arg_spans: generic_args.iter().map(HasSpan::span).collect(),
        };

        let matches_generic_from_surroundings =
            lowerer.in_scope_generic_params.and_then(|generic_params| {
                generic_params
                    .iter()
                    .copied()
                    .enumerate()
                    .find(|(_, param)| param.symbol == ident.symbol)
            });

        if let Some((def_index, def_ident)) = matches_generic_from_surroundings {
            // Name of the type was found in the in-scope generic parameters,
            // so it's a type parameter.
            //
            // Yes, this takes precedence over primitives. You can do
            // `struct Wrap I32 = I32` and the `I32` on the right will refer
            // to the generic type and not the primitive.
            //
            // It's a generic type! So it shouldn't take any arguments.
            if !generic_args.is_empty() {
                lowerer
                    .errors
                    .push(err(UnexpectedTypeArgs::GenericParam { def_ident }));
            }

            TypeKind::Generic(def_ident, def_index as u32)
        } else if let Ok(prim) = self.path.ident.literal.parse() {
            // Primitives shouldn't take any generic arguments.
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
