use crate::error::RegionError;
use crate::{LoweringError, UnexpectedTypeArgs};
use bumpalo::Bump;
use bumpalo_thin_slice::{BumpaloThinSliceExt, ThinSlice};
use curse_ast::ast;
use curse_hir::hir::{
    Appl, Arm, Binding, ChoiceDef, Constructor, Expr, ExprKind, FunctionDef, Lit, Param, Pat,
    PatKind, Program, Region, RegionKind, StructDef, Symbol, Type, TypeKind,
};
use curse_interner::{Ident, InternedString};
use curse_span::HasSpan;
use std::{collections::HashMap, slice};

trait CollectIntoThinSlice<T>
where
    Self: Sized + IntoIterator<Item = T>,
    Self::IntoIter: ExactSizeIterator,
{
    fn collect_thin_slice(self, bump: &Bump) -> ThinSlice<'_, T> {
        bump.alloc_thin_slice_fill_iter(self).into_thin_slice()
    }
}

impl<I, T> CollectIntoThinSlice<T> for I
where
    I: Sized + IntoIterator<Item = T>,
    I::IntoIter: ExactSizeIterator,
{
}

pub trait Lower<'hir> {
    type Lowered;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered;

    fn lower_and_alloc(&self, lowerer: &mut Lowerer<'hir>) -> &'hir Self::Lowered {
        lowerer.bump.alloc(self.lower(lowerer))
    }
}

pub struct Lowerer<'hir> {
    pub bump: &'hir Bump,
    in_scope_generic_params: Option<ThinSlice<'hir, Ident>>,
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
    fn with_generic_params<F, R>(&mut self, generic_params: ThinSlice<'hir, Ident>, f: F) -> R
    where
        F: FnOnce(&mut Lowerer<'hir>) -> R,
    {
        let outer_generics = self.in_scope_generic_params.replace(generic_params);
        let res = f(self);
        self.in_scope_generic_params = outer_generics;
        res
    }
}

impl<'hir> Lower<'hir> for ast::Program {
    type Lowered = Program<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        use std::collections::hash_map::Entry;

        /// Inserts the lowered def into the map if the name is unique,
        /// otherwise leaves the original and pushes a `LoweringError`
        /// describing that there are multiple defs with the same name.
        fn insert_or_push_err<T: HasSpan>(
            lowered: T,
            entry: Entry<'_, InternedString, T>,
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
            dynamic_imports: self.dynamic_imports.iter().map(|di| di.module).collect(),
        };

        for ast_def in self.function_defs.iter() {
            let def = ast_def.lower(lowerer);
            let entry = program.function_defs.entry(def.ident.symbol);
            insert_or_push_err(def, entry, &mut lowerer.errors);
        }

        for def in self.struct_defs.iter() {
            let def = def.lower(lowerer);
            let entry = program.struct_defs.entry(def.ident.symbol);
            insert_or_push_err(def, entry, &mut lowerer.errors)
        }

        for def in self.choice_defs.iter() {
            let def = def.lower(lowerer);
            let entry = program.choice_defs.entry(def.ident.symbol);
            insert_or_push_err(def, entry, &mut lowerer.errors)
        }

        program
    }
}

impl<'hir> Lower<'hir> for ast::StructDef {
    type Lowered = StructDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let generic_params = self
            .generic_params
            .as_ref()
            .map(|generic_params| {
                generic_params
                    .params
                    .iter()
                    .copied()
                    .collect_thin_slice(lowerer.bump)
            })
            .unwrap_or_default();

        let ty = lowerer.with_generic_params(generic_params, |lowerer| self.ty.lower(lowerer));

        StructDef {
            ident: self.ident,
            generic_params,
            ty,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::ChoiceDef {
    type Lowered = ChoiceDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let generic_params = self
            .generic_params
            .as_ref()
            .map(|generic_params| {
                generic_params
                    .params
                    .iter()
                    .copied()
                    .collect_thin_slice(lowerer.bump)
            })
            .unwrap_or_default();

        let variants = lowerer.with_generic_params(generic_params, |lowerer| {
            let bump = lowerer.bump;
            self.variants
                .variants
                .iter()
                .map(|variant| (variant.ident, variant.ty.lower(lowerer)))
                .collect_thin_slice(bump)
        });

        ChoiceDef {
            ident: self.ident,
            generic_params,
            variants,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::FunctionDef {
    type Lowered = FunctionDef<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        // TODO(quinn): put this code back in once we have explicit types again
        // let (generic_params, ty) = self
        //     .explicit_types
        //     .as_ref()
        //     .map(|explicit_types| {
        //         let generic_params = explicit_types
        //             .generic_params
        //             .as_ref()
        //             .map(|x| x.lower(lowerer))
        //             .unwrap_or_default();
        //
        //         let ty = explicit_types.ty.lower(lowerer);
        //         let ty = Some(&*lowerer.bump.alloc(ty));
        //
        //         (generic_params, ty)
        //     })
        //     .unwrap_or_default();

        let (generic_params, ty) = Default::default();

        let arms =
            lowerer.with_generic_params(generic_params, |lowerer| self.function.lower(lowerer));

        FunctionDef {
            ident: self.ident,
            generic_params,
            ty,
            arms,
            span: self.span(),
        }
    }
}

// impl<'hir> IntoHir<'hir, ThinSlice<'hir, Ident>> for ast::GenericParams {
//     fn into_hir(&self, lowerer: &mut Lowerer<'hir>) -> ThinSlice<'hir, Ident> {
//         self.params.iter().copied().collect_thin_slice(lowerer.bump)
//     }
// }

impl<'hir> Lower<'hir> for ast::Expr {
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
            ast::Expr::Record(record) => ExprKind::Record(record.as_ref().lower(lowerer)),
            ast::Expr::Constructor(constructor) => {
                ExprKind::Constructor(constructor.as_ref().lower_and_alloc(lowerer))
            }
            ast::Expr::Closure(closure) => ExprKind::Closure(closure.as_ref().lower(lowerer)),
            ast::Expr::Appl(appl) => ExprKind::Appl(lowerer.bump.alloc(Appl {
                lhs: appl.lhs.lower(lowerer),
                fun: appl.fun.lower(lowerer),
                rhs: appl.rhs.lower(lowerer),
            })),
            ast::Expr::Region(region) => ExprKind::Region(region.as_ref().lower_and_alloc(lowerer)),
            // ast::Expr::Field(_expr, _field) => todo!("lowering fields"),
            ast::Expr::Error => ExprKind::Error,
        };

        Expr {
            span: self.span(),
            kind,
        }
    }
}

impl<'hir> Lower<'hir> for ast::Lit {
    type Lowered = Result<Lit, ()>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        match self {
            ast::Lit::Integer(integer) => parse_u32(&integer.symbol.string())
                .map(Lit::Integer)
                .ok_or_else(|| {
                    lowerer.errors.push(LoweringError::IntegerLiteralOverflow {
                        literal: integer.to_string(),
                        span: integer.span(),
                    });
                }),
            ast::Lit::Ident(ident) => Ok(Lit::Ident(ident.symbol)),
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

impl<'hir, Kind> Lower<'hir> for ast::Constructor<Kind>
where
    Kind: Lower<'hir>,
    Kind::Lowered: 'hir,
{
    type Lowered = Constructor<'hir, Kind::Lowered>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = self.inner.lower_and_alloc(lowerer);

        Constructor {
            ty: self.ty,
            variant: self.variant,
            kind,
        }
    }
}

impl<'hir> Lower<'hir> for ast::Closure {
    type Lowered = ThinSlice<'hir, Arm<'hir>>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let bump = lowerer.bump;
        self.arms()
            .iter()
            .map(|arm| {
                if arm.params.len() > 2 {
                    lowerer.errors.push(LoweringError::TooManyClosureParams {
                        all_params: arm.params.iter().map(HasSpan::span).collect(),
                    });
                }

                Arm {
                    params: arm
                        .params
                        .iter()
                        .map(|param| Param {
                            pat: param.pat.lower(lowerer),
                            ascription: param.ascription.lower(lowerer),
                        })
                        .collect_thin_slice(bump),
                    body: arm.body.lower(lowerer),
                }
            })
            .collect_thin_slice(bump)
    }
}

impl<'hir> Lower<'hir> for ast::Region {
    type Lowered = Region<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self.kind {
            ast::RegionKind::Ref(_) => RegionKind::Ref,
            ast::RegionKind::Mut(_) => RegionKind::Mut,
            ast::RegionKind::RefMut(_, _) => RegionKind::RefMut,
        };

        let body = self.body.lower(lowerer);

        let shadows = match &self.pat {
            ast::Pat::Lit(lit) => match lit {
                ast::Lit::Ident(ident) => Ok(slice::from_ref(lowerer.bump.alloc(*ident))),
                ast::Lit::Integer(int) => Err((RegionError::LiteralNumber, int.span())),
                ast::Lit::True(tru) => Err((RegionError::LiteralTrue, tru.span())),
                ast::Lit::False(fals) => Err((RegionError::LiteralFalse, fals.span())),
            }
            .unwrap_or_else(|(err, span)| {
                lowerer.errors.push(LoweringError::Region(err, span));
                &[]
            }),
            ast::Pat::Record(_record) => {
                todo!("Regions are on hold for now")
                // lowerer
                //     .bump
                //     .alloc_slice_fill_iter(record.iter_fields().map(|field| {
                //
                //         // What would this even mean?
                //         // mut {
                //         //     { a, b: { e, f } }: c,
                //         // } { .. }
                //         // I guess it would mean "return a record { a, e, f } that
                //         // will overwrite c.a and c.b.e and c.b.f fields"
                //         // THAT'S SO COOL!
                //
                //         if let Some((_colon, pat)) = field.value.as_ref() {
                //             lowerer.errors.push(LoweringError::Region(
                //                 RegionError::RecordWithValue,
                //                 pat.span(),
                //             ));
                //         }
                //         Ident::from(field.ident)
                //     }))
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

impl<'hir> Lower<'hir> for ast::Pat {
    type Lowered = Pat<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self {
            ast::Pat::Lit(lit) => match lit.lower(lowerer) {
                Ok(lit) => PatKind::Lit(lit),
                Err(()) => PatKind::Error,
            },
            ast::Pat::Record(record) => PatKind::Record(record.as_ref().lower(lowerer)),
            ast::Pat::Constructor(constructor) => {
                PatKind::Constructor(constructor.as_ref().lower_and_alloc(lowerer))
            }
        };

        Pat {
            kind,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::Type {
    type Lowered = Type<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let kind = match self {
            ast::Type::Named(named) => named.lower(lowerer),
            ast::Type::Record(record) => TypeKind::Record(record.as_ref().lower(lowerer)),
            ast::Type::Error => TypeKind::Error,
        };

        Type {
            kind,
            span: self.span(),
        }
    }
}

impl<'hir> Lower<'hir> for ast::NamedType {
    type Lowered = TypeKind<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let bump = lowerer.bump;
        let generic_args = self
            .generic_args
            .as_ref()
            .map(|args| {
                args.types()
                    .iter()
                    .map(|ty| ty.lower(lowerer))
                    .collect_thin_slice(bump)
            })
            .unwrap_or_default();

        let path = self
            .path
            .parts
            .iter()
            .copied()
            .collect_thin_slice(lowerer.bump);

        let [ident] = path.as_slice() else {
            // More than 1 item in the path, can't be a generic or a primitive
            // Note: path cannot have 0 elements since `ast::Path` has 1 inlined.
            return TypeKind::Named { path, generic_args };
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

            return TypeKind::Generic {
                name: def_ident,
                index: def_index as u32,
            };
        }

        if let [part] = self.path.parts[..] {
            if let Ok(prim) = part.symbol.string().parse() {
                // Primitives shouldn't take any generic arguments.
                if !generic_args.is_empty() {
                    lowerer
                        .errors
                        .push(err(UnexpectedTypeArgs::Primitive(prim)));
                }

                return TypeKind::Primitive(prim);
            }
        }

        TypeKind::Named { path, generic_args }
    }
}

// impl<'hir> Lower<'hir> for ast::FieldBinding {
//     type Lowered = FieldBinding<'hir>;
//
//     fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
//         match self {
//             ast::FieldBinding::Binding(ident) => FieldBinding::Binding(*ident),
//             ast::FieldBinding::Tree(_, tree, _) => {
//                 FieldBinding::Tree(lowerer.bump.alloc_slice_fill_iter(tree.iter().map(
//                     |(ident, maybe_colon_binding)| {
//                         (
//                             *ident,
//                             maybe_colon_binding
//                                 .as_ref()
//                                 .map(|(_colon, binding)| binding.lower(lowerer)),
//                         )
//                     },
//                 )))
//             }
//         }
//     }
// }
//
// impl<'hir> Lower<'hir> for ast::Record<ast::Type> {
//     type Lowered = &'hir [BindingAndValue<'hir, Type<'hir>>];
//
//     fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
//         lowerer
//             .bump
//             .alloc_slice_fill_iter(self.iter_fields().map(|field| match field {
//                 ast::FieldSyntax::Shorthand(ident) => {
//                     // Error: record types can't omit the type
//                     lowerer
//                         .errors
//                         .push(LoweringError::TypeRecordMissingFieldType {
//                             field_ident: *ident,
//                         });
//
//                     let binding = FieldBinding::Binding(*ident);
//                     let value = Type {
//                         kind: TypeKind::Error,
//                         span: ident.span(),
//                     };
//
//                     BindingAndValue { binding, value }
//                 }
//                 ast::FieldSyntax::PatAndValue(binding, _, value) => {
//                     let binding = binding.lower(lowerer);
//                     let value = value.lower(lowerer);
//
//                     BindingAndValue { binding, value }
//                 }
//             }))
//     }
// }

impl<'hir> Lower<'hir> for ast::Record<ast::Expr> {
    type Lowered = ThinSlice<'hir, (Binding<'hir>, Option<Expr<'hir>>)>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let bump = lowerer.bump;
        self.fields
            .iter()
            .map(|(binding, opt_expr)| {
                let binding = binding.lower(lowerer);
                let opt_expr = opt_expr.lower(lowerer);

                (binding, opt_expr)
            })
            .collect_thin_slice(bump)
    }
}

impl<'hir> Lower<'hir> for ast::Record<ast::Pat> {
    type Lowered = ThinSlice<'hir, (Binding<'hir>, Option<Pat<'hir>>)>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let bump = lowerer.bump;
        self.fields
            .iter()
            .map(|(binding, opt_pat)| {
                let binding = binding.lower(lowerer);
                let opt_pat = opt_pat.lower(lowerer);

                (binding, opt_pat)
            })
            .collect_thin_slice(bump)
    }
}

impl<'hir> Lower<'hir> for ast::Record<ast::Type> {
    type Lowered = ThinSlice<'hir, (Binding<'hir>, Type<'hir>)>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let bump = lowerer.bump;
        self.fields
            .iter()
            .map(|(binding, opt_ty)| {
                let ty = opt_ty.lower(lowerer).unwrap_or_else(|| {
                    lowerer
                        .errors
                        .push(LoweringError::TypeRecordMissingFieldType {
                            field_binding: binding.clone(),
                        });

                    Type::error(binding.span())
                });
                let binding = binding.lower(lowerer);

                (binding, ty)
            })
            .collect_thin_slice(bump)
    }
}

impl<'hir> Lower<'hir> for ast::Binding {
    type Lowered = Binding<'hir>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        let bump = lowerer.bump;
        match self {
            ast::Binding::Ident(ident) => Binding::Ident(*ident),
            ast::Binding::Record(record) => Binding::Record(
                record
                    .fields
                    .iter()
                    .map(|(binding, opt_binding)| {
                        let binding = binding.lower(lowerer);
                        let opt_binding = opt_binding.lower(lowerer);

                        (binding, opt_binding)
                    })
                    .collect_thin_slice(bump),
            ),
        }
    }
}

impl<'hir, T> Lower<'hir> for Option<T>
where
    T: Lower<'hir>,
    T::Lowered: 'hir,
{
    type Lowered = Option<T::Lowered>;

    fn lower(&self, lowerer: &mut Lowerer<'hir>) -> Self::Lowered {
        self.as_ref().map(|val| val.lower(lowerer))
    }
}
