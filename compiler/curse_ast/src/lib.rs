//! The Curse abstract-syntax tree (AST).

pub mod arena;
pub mod ast;
pub mod visit {
    use crate::ast::{self, Variants};

    pub trait Visit {
        fn visit_program(&mut self, _program: &ast::Program<'_, '_>) {}

        fn visit_function_def(&mut self, _function_def: &ast::FunctionDef<'_, '_>) {}

        fn visit_struct_def(&mut self, _struct_def: &ast::StructDef<'_, '_>) {}

        fn visit_choice_def(&mut self, _choice_def: &ast::ChoiceDef<'_, '_>) {}

        fn visit_explicit_types(&mut self, _explicit_types: &ast::ExplicitTypes<'_, '_>) {}

        fn visit_generic_params(&mut self, _generic_params: &ast::GenericParams<'_>) {}

        fn visit_appl(&mut self, _appl: &ast::Appl<'_, '_>) {}

        fn visit_arm(&mut self, _arm: &ast::Arm<'_, '_>) {}

        fn visit_closure(&mut self, _closure: &ast::Closure<'_, '_>) {}

        fn visit_expr(&mut self, _expr: &ast::Expr<'_, '_>) {}

        fn visit_param(&mut self, _param: &ast::Param<'_, '_>) {}

        fn visit_paren(&mut self, _paren: &ast::Paren<'_, '_>) {}

        fn visit_symbol(&mut self, _symbol: &ast::Symbol) {}

        fn visit_expr_record(&mut self, _record: &ast::Record<'_, ast::ExprRef<'_, '_>>) {}

        fn visit_expr_field(&mut self, _field: &ast::Field<'_, ast::ExprRef<'_, '_>>) {}

        fn visit_pat(&mut self, _pat: &ast::Pat<'_, '_>) {}

        fn visit_pat_record(&mut self, _record: &ast::Record<'_, ast::PatRef<'_, '_>>) {}

        fn visit_pat_field(&mut self, _field: &ast::Field<'_, ast::PatRef<'_, '_>>) {}

        fn visit_expr_constructor(
            &mut self,
            _constructor: &ast::Constructor<'_, '_, ast::Expr<'_, '_>>,
        ) {
        }

        fn visit_pat_constructor(
            &mut self,
            _constructor: &ast::Constructor<'_, '_, ast::Pat<'_, '_>>,
        ) {
        }

        fn visit_lit(&mut self, _lit: &ast::Lit<'_>) {}

        fn visit_path(&mut self, _path: &ast::Path<'_>) {}

        fn visit_generic_args(&mut self, _generic_args: &ast::GenericArgs<'_, '_>) {}

        fn visit_named_type(&mut self, _named_type: &ast::NamedType<'_, '_>) {}

        fn visit_type(&mut self, _ty: &ast::Type<'_, '_>) {}

        fn visit_type_record(&mut self, _record: &ast::Record<'_, ast::TypeRef<'_, '_>>) {}

        fn visit_type_field(&mut self, _field: &ast::Field<'_, ast::TypeRef<'_, '_>>) {}
    }

    /// A [`Visit`] type that wraps an inner [`Visit`] type and walks the AST,
    /// visiting each item with the inner visitor.
    pub struct Walker<V> {
        pub visitor: V,
    }

    impl<V: Visit> Walker<V> {
        pub fn new(visitor: V) -> Self {
            Walker { visitor }
        }
    }

    impl<V: Visit> Visit for Walker<V> {
        fn visit_program(&mut self, program: &ast::Program<'_, '_>) {
            self.visitor.visit_program(program);
            for def in program.function_defs.iter() {
                self.visit_function_def(def);
            }
            for def in program.struct_defs.iter() {
                self.visit_struct_def(def);
            }
            for def in program.choice_defs.iter() {
                self.visit_choice_def(def);
            }
        }

        fn visit_function_def(&mut self, function_def: &ast::FunctionDef<'_, '_>) {
            self.visitor.visit_function_def(function_def);
            if let Some(ref explicit_types) = function_def.explicit_types {
                self.visit_explicit_types(explicit_types);
            }
            self.visit_closure(&function_def.function);
        }

        fn visit_struct_def(&mut self, struct_def: &ast::StructDef<'_, '_>) {
            self.visitor.visit_struct_def(struct_def);
            if let Some(ref generic_params) = struct_def.generic_params {
                self.visit_generic_params(generic_params);
            }
            self.visit_type(struct_def.ty);
        }

        fn visit_choice_def(&mut self, choice_def: &ast::ChoiceDef<'_, '_>) {
            self.visitor.visit_choice_def(choice_def);
            if let Some(ref generic_params) = choice_def.generic_params {
                self.visit_generic_params(generic_params);
            }
            if let Variants::Variants(_pipe, variants, last) = &choice_def.variants {
                variants
                    .iter()
                    .map(|(variant, _pipe)| variant)
                    .chain(Some(last))
                    .for_each(|variant| self.visit_type(variant.ty));
            }
        }

        fn visit_explicit_types(&mut self, explicit_types: &ast::ExplicitTypes<'_, '_>) {
            self.visitor.visit_explicit_types(explicit_types);
            if let Some(ref generic_params) = explicit_types.generic_params {
                self.visit_generic_params(generic_params);
            }
            self.visit_type(explicit_types.ty);
        }

        fn visit_generic_params(&mut self, generic_params: &ast::GenericParams<'_>) {
            self.visitor.visit_generic_params(generic_params);
        }

        fn visit_appl(&mut self, appl: &ast::Appl<'_, '_>) {
            self.visitor.visit_appl(appl);
            self.visit_expr(appl.lhs);
            self.visit_expr(appl.fun);
            self.visit_expr(appl.rhs);
        }

        fn visit_arm(&mut self, arm: &ast::Arm<'_, '_>) {
            self.visitor.visit_arm(arm);
            arm.params
                .iter()
                .map(|(param, _comma)| param)
                .chain(arm.last.as_ref())
                .for_each(|param| self.visit_param(param));
            self.visit_expr(arm.body);
        }

        fn visit_closure(&mut self, closure: &ast::Closure<'_, '_>) {
            self.visitor.visit_closure(closure);
            for arm in closure.arms() {
                self.visit_arm(arm);
            }
        }

        fn visit_expr(&mut self, expr: &ast::Expr<'_, '_>) {
            self.visitor.visit_expr(expr);
            match expr {
                ast::Expr::Paren(paren) => self.visit_paren(paren),
                ast::Expr::Symbol(symbol) => self.visit_symbol(symbol),
                ast::Expr::Lit(lit) => self.visit_lit(lit),
                ast::Expr::Record(record) => self.visit_expr_record(record),
                ast::Expr::Constructor(constructor) => self.visit_expr_constructor(constructor),
                ast::Expr::Closure(closure) => self.visit_closure(closure),
                ast::Expr::Appl(appl) => self.visit_appl(appl),
                ast::Expr::Error => {}
            }
        }

        fn visit_param(&mut self, param: &ast::Param<'_, '_>) {
            self.visitor.visit_param(param);
            self.visit_pat(param.pat);
            if let Some((_colon, ty)) = param.ascription {
                self.visit_type(ty);
            }
        }

        fn visit_paren(&mut self, paren: &ast::Paren<'_, '_>) {
            self.visitor.visit_paren(paren);
            self.visit_expr(paren.expr);
        }

        fn visit_symbol(&mut self, symbol: &ast::Symbol) {
            self.visitor.visit_symbol(symbol);
        }

        fn visit_expr_record(&mut self, record: &ast::Record<'_, ast::ExprRef<'_, '_>>) {
            self.visitor.visit_expr_record(record);
            for field in record.fields() {
                self.visit_expr_field(field);
            }
        }

        fn visit_expr_field(&mut self, field: &ast::Field<'_, ast::ExprRef<'_, '_>>) {
            self.visitor.visit_expr_field(field);
            if let Some((_colon, expr)) = field.value {
                self.visit_expr(expr);
            }
        }

        fn visit_pat(&mut self, pat: &ast::Pat<'_, '_>) {
            self.visitor.visit_pat(pat);
            match pat {
                ast::Pat::Lit(lit) => self.visit_lit(lit),
                ast::Pat::Record(record) => self.visit_pat_record(record),
                ast::Pat::Constructor(constructor) => self.visit_pat_constructor(constructor),
            }
        }

        fn visit_pat_record(&mut self, record: &ast::Record<'_, ast::PatRef<'_, '_>>) {
            self.visitor.visit_pat_record(record);
            for field in record.fields() {
                self.visit_pat_field(field);
            }
        }

        fn visit_pat_field(&mut self, field: &ast::Field<'_, ast::PatRef<'_, '_>>) {
            self.visitor.visit_pat_field(field);
            if let Some((_colon, pat)) = field.value {
                self.visit_pat(pat);
            }
        }

        fn visit_expr_constructor(
            &mut self,
            constructor: &ast::Constructor<'_, '_, ast::Expr<'_, '_>>,
        ) {
            self.visitor.visit_expr_constructor(constructor);
            self.visit_path(&constructor.path);
            self.visit_expr(constructor.inner);
        }

        fn visit_pat_constructor(
            &mut self,
            constructor: &ast::Constructor<'_, '_, ast::Pat<'_, '_>>,
        ) {
            self.visitor.visit_pat_constructor(constructor);
            self.visit_path(&constructor.path);
            self.visit_pat(constructor.inner);
        }

        fn visit_lit(&mut self, lit: &ast::Lit<'_>) {
            self.visitor.visit_lit(lit);
        }

        fn visit_path(&mut self, path: &ast::Path<'_>) {
            self.visitor.visit_path(path);
        }

        fn visit_generic_args(&mut self, generic_args: &ast::GenericArgs<'_, '_>) {
            self.visitor.visit_generic_args(generic_args);
            match generic_args {
                ast::GenericArgs::Single(ty) => self.visit_type(ty),
                ast::GenericArgs::CartesianProduct(_, types, last, _) => {
                    for (ty, _star) in types {
                        self.visit_type(ty);
                    }
                    self.visit_type(last);
                }
            }
        }

        fn visit_named_type(&mut self, named_type: &ast::NamedType<'_, '_>) {
            self.visitor.visit_named_type(named_type);
            self.visit_path(&named_type.path);
            if let Some(ref generic_args) = named_type.generic_args {
                self.visit_generic_args(generic_args);
            }
        }

        fn visit_type(&mut self, ty: &ast::Type<'_, '_>) {
            self.visitor.visit_type(ty);
            match ty {
                ast::Type::Named(named) => self.visit_named_type(named),
                ast::Type::Record(record) => self.visit_type_record(record),
                ast::Type::Error => {}
            }
        }

        fn visit_type_record(&mut self, record: &ast::Record<'_, ast::TypeRef<'_, '_>>) {
            self.visitor.visit_type_record(record);
            for field in record.fields() {
                self.visit_type_field(field);
            }
        }

        fn visit_type_field(&mut self, field: &ast::Field<'_, ast::TypeRef<'_, '_>>) {
            self.visitor.visit_type_field(field);
            if let Some((_colon, ty)) = field.value {
                self.visit_type(ty);
            }
        }
    }
}
