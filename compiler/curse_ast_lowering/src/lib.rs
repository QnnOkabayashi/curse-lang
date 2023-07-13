//! Lowers the AST to the HIR.

mod arena_alloc;
mod error;
mod lowerer;

pub use counter::make_hir_arena;
pub use error::{LoweringError, UnexpectedTypeArgs};
pub use lowerer::{Lower, Lowerer};

mod counter {
    use curse_arena::new::Arena;
    use curse_ast::ast;
    use curse_ast::visit::{Visit, Walker};
    use curse_hir::arena::HirArena;

    /// Traverses the program and counts exactly how many allocations are required,
    /// and constructs an [`HirArena`] with the exact preallocated capacity.
    pub fn make_hir_arena<'hir>(program: &ast::Program<'_, '_>) -> HirArena<'hir> {
        let mut walker = Walker::new(AllocationCounter::default());

        walker.visit_program(program);

        let counter = walker.visitor;

        HirArena {
            idents: Arena::with_capacity(counter.idents),
            ident_and_type: Arena::with_capacity(counter.ident_and_type),
            exprs: Arena::with_capacity(counter.exprs),
            ident_and_opt_expr: Arena::with_capacity(counter.ident_and_opt_expr),
            arms: Arena::with_capacity(counter.arms),
            params: Arena::with_capacity(counter.params),
            pats: Arena::with_capacity(counter.pats),
            ident_and_opt_pat: Arena::with_capacity(counter.ident_and_opt_pat),
            types: Arena::with_capacity(counter.types),
        }
    }

    #[derive(Default)]
    struct AllocationCounter {
        pub idents: usize,
        pub ident_and_type: usize,
        pub exprs: usize,
        pub ident_and_opt_expr: usize,
        pub arms: usize,
        pub params: usize,
        pub pats: usize,
        pub ident_and_opt_pat: usize,
        pub types: usize,
    }

    impl Visit for AllocationCounter {
        fn visit_choice_def(&mut self, choice_def: &ast::ChoiceDef<'_, '_>) {
            self.ident_and_type += choice_def.variants.len();
        }

        fn visit_generic_params(&mut self, generic_params: &ast::GenericParams<'_>) {
            self.idents += generic_params.len();
        }

        fn visit_arm(&mut self, _arm: &ast::Arm<'_, '_>) {
            self.arms += 1;
        }

        fn visit_expr(&mut self, _expr: &ast::Expr<'_, '_>) {
            self.exprs += 1;
        }

        fn visit_param(&mut self, _param: &ast::Param<'_, '_>) {
            self.params += 1;
        }

        fn visit_expr_field(&mut self, _field: &ast::Field<'_, ast::ExprRef<'_, '_>>) {
            self.ident_and_opt_expr += 1;
        }

        fn visit_pat(&mut self, _pat: &ast::Pat<'_, '_>) {
            self.pats += 1;
        }

        fn visit_pat_field(&mut self, _field: &ast::Field<'_, ast::PatRef<'_, '_>>) {
            self.ident_and_opt_pat += 1;
        }

        fn visit_path(&mut self, path: &ast::Path<'_>) {
            self.idents += path.len();
        }

        fn visit_type(&mut self, _ty: &ast::Type<'_, '_>) {
            self.types += 1;
        }

        fn visit_type_field(&mut self, _field: &ast::Field<'_, ast::TypeRef<'_, '_>>) {
            self.ident_and_type += 1;
        }
    }
}
