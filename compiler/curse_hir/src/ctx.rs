use ast::tok;
use curse_ast::{self as ast, Span};
use smallvec::smallvec;
use std::{
    cell::RefCell,
    collections::HashMap,
    iter,
    ops::{Index, IndexMut},
};
use string_interner::{DefaultSymbol, StringInterner};
use typed_arena::Arena;

use crate::{
    defs, expr, pat, types, Equations, Type, TypeFunction, TypeIdent, TypeKind, TypeSymbol,
    TypeTemplate, Typevar, ValueIdent, ValueSymbol, Var,
};

#[derive(Default)]
pub struct Global<'cx> {
    pub string_interner: RefCell<StringInterner>,
    pub type_fns: Arena<types::TypeFunction<'cx>>,
    pub types: Arena<types::Type<'cx>>,
    pub pats: Arena<pat::Pat<'cx>>,
    pub appls: Arena<expr::ExprAppl<'cx>>,
    pub arms: Arena<expr::ExprArm<'cx>>,
    pub exprs: Arena<expr::Expr<'cx>>,

    // For slices only
    pub type_idents: Arena<TypeIdent>,
    pub value_idents: Arena<ValueIdent>,

    pub defs: DefArena<'cx>,
}

#[derive(Default)]
pub struct DefArena<'cx> {
    pub named_fields: Arena<defs::NamedField<'cx>>,
    pub field_types: Arena<defs::FieldType<'cx>>,
    pub variant_defs: Arena<defs::VariantDef<'cx>>,
}

pub struct Typeck<'cx> {
    pub global: &'cx Global<'cx>,
    pub typevars: Vec<Typevar<'cx>>,
    pub equations: Equations<'cx>,
}

pub struct Defs<'cx> {
    pub global: &'cx Global<'cx>,
    pub struct_defs: HashMap<TypeSymbol, defs::StructDef<'cx>>,
    pub choice_defs: HashMap<TypeSymbol, defs::ChoiceDef<'cx>>,
}

impl<'cx> Global<'cx> {
    fn get_or_intern(&self, string: &str) -> DefaultSymbol {
        self.string_interner.borrow_mut().get_or_intern(string)
    }

    pub fn get_or_intern_type_ident(&self, ident: &tok::TypeIdent<'_>) -> TypeIdent {
        TypeIdent {
            symbol: TypeSymbol(self.get_or_intern(ident.literal)),
            location: ident.location,
        }
    }

    pub fn get_or_intern_value_ident(&self, ident: &tok::Ident<'_>) -> ValueIdent {
        ValueIdent {
            symbol: ValueSymbol(self.get_or_intern(ident.literal)),
            location: ident.location,
        }
    }
}

impl<'cx> Typeck<'cx> {
    pub fn with_global(global: &'cx Global<'cx>) -> Self {
        Typeck {
            global,
            typevars: Vec::new(),
            equations: Equations::new(),
        }
    }

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
    pub fn default_globals(&mut self) -> impl Iterator<Item = (ValueSymbol, TypeTemplate<'cx>)> {
        let dummy = (0, 0);
        [
            (ValueSymbol(self.global.get_or_intern("in")), {
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

                TypeTemplate {
                    typevars: smallvec![x, y],
                    ty: Type {
                        kind: TypeKind::Function(self.global.type_fns.alloc(TypeFunction {
                            lhs: x_type,
                            rhs: Type {
                                kind: TypeKind::Function(self.global.type_fns.alloc(
                                    TypeFunction {
                                        lhs: x_type,
                                        rhs: Type {
                                            kind: TypeKind::unit(),
                                            span: dummy,
                                        },
                                        output: y_type,
                                    },
                                )),
                                span: dummy,
                            },
                            output: y_type,
                        })),
                        span: dummy,
                    },
                }
            }),
            (ValueSymbol(self.global.get_or_intern("print")), {
                let x = self.new_typevar();
                let x_type = Type {
                    kind: TypeKind::Var(x),
                    span: dummy,
                };

                TypeTemplate {
                    typevars: smallvec![x],
                    ty: Type {
                        kind: TypeKind::Function(self.global.type_fns.alloc(TypeFunction {
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

    pub fn monomorphize(&mut self, template: &TypeTemplate<'cx>) -> Type<'cx> {
        // Takes a polymorphic type and replaces all instances of generics
        // with a fixed, unbound type.
        // For example, id: T -> T is a polymorphic type, so it goes through
        // and replaces both `T`s with an unbound type variable like `a0`,
        // which is then bound later on.
        fn replace_unbound_typevars<'cx>(
            tbl: &HashMap<Var, TypeKind<'cx>>,
            hir: &'cx Global<'cx>,
            ty: Type<'cx>,
        ) -> Type<'cx> {
            match ty.kind {
                TypeKind::Var(var) => Type {
                    kind: tbl[&var],
                    ..ty
                },
                TypeKind::Function(fun) => Type {
                    kind: TypeKind::Function(hir.type_fns.alloc(TypeFunction {
                        lhs: replace_unbound_typevars(tbl, hir, fun.lhs),
                        rhs: replace_unbound_typevars(tbl, hir, fun.rhs),
                        output: replace_unbound_typevars(tbl, hir, fun.output),
                    })),
                    ..ty
                },
                TypeKind::Record(types) => {
                    let replaced_types = hir
                        .types
                        .alloc_extend(iter::repeat_with(Type::default).take(types.len()));

                    for (slot, ty) in replaced_types.iter_mut().zip(types) {
                        *slot = replace_unbound_typevars(tbl, hir, *ty);
                    }

                    Type {
                        kind: TypeKind::Record(replaced_types),
                        ..ty
                    }
                }
                _ => ty,
            }
        }

        let tvs_to_replace = template
            .typevars
            .iter()
            .map(|tv| (*tv, TypeKind::Var(self.new_typevar())))
            .collect();

        replace_unbound_typevars(&tvs_to_replace, self.global, template.ty)
    }

    /// Convert an [`ast::Type<'_, 'input>`] annotation into an HIR [`Type<'cx>`].
    pub fn type_from_ast(
        &mut self,
        ty: &ast::Type<'_, '_>,
        map: &HashMap<TypeSymbol, Type<'cx>>,
    ) -> Type<'cx> {
        match ty {
            // TODO(quinn): Should do a trie implementation for types?
            // Because types are basically trees...
            ast::Type::Named(named) => match named.name.literal {
                "I32" => Type {
                    kind: TypeKind::I32,
                    span: named.span(),
                },
                "Bool" => Type {
                    kind: TypeKind::Bool,
                    span: named.span(),
                },
                // TODO(quinn): for now this doesn't support generic types at all
                _ => map
                    .get(&self.global.get_or_intern_type_ident(&named.name).symbol)
                    .copied()
                    .expect("type not found"),
            },
            ast::Type::Tuple(tuple) => {
                let types = self
                    .global
                    .types
                    .alloc_extend(iter::repeat_with(Type::default).take(tuple.len()));

                for (i, ty) in tuple.iter_elements().enumerate() {
                    types[i] = self.type_from_ast(ty, map);
                }

                Type {
                    kind: TypeKind::Record(types),
                    span: tuple.span(),
                }
            }
            ast::Type::Error => todo!("Handle errors :)"),
            // ast::Type::Function(fun) => {
            //     let lhs = self.type_from_ast(fun.lhs, map);
            //     let rhs = self.type_from_ast(fun.rhs, map);
            //     let output = self.type_from_ast(fun.output, map);
            //
            //     Type {
            //         kind: TypeKind::Function(self.global.type_fns.alloc(TypeFunction {
            //             lhs,
            //             rhs,
            //             output,
            //         })),
            //         span: fun.span(),
            //     }
            // }
        }
    }

    pub fn occurs(&self, var: Var, ty: &Type<'_>) -> bool {
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

    pub fn check_equivalence(&self, var: Var, ty: Type<'_>) -> bool {
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

impl<'cx> Index<Var> for Typeck<'cx> {
    type Output = Typevar<'cx>;

    fn index(&self, index: Var) -> &Self::Output {
        &self.typevars[index.0]
    }
}

impl<'cx> IndexMut<Var> for Typeck<'cx> {
    fn index_mut(&mut self, index: Var) -> &mut Self::Output {
        &mut self.typevars[index.0]
    }
}
