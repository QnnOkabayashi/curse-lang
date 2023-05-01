#![allow(dead_code)]
use curse_ast::Span;
use curse_hir as hir;
use curse_parse as parse;
use hir::Ty;
use miette::{GraphicalReportHandler, NamedSource};
use smallvec::SmallVec;
use std::collections::HashMap;
use typed_arena::Arena;

mod programs;

fn main() {
    let input: &str = programs::NESTED_CLOSURES;

    let ast = parse::Ast::new();
    let program = match parse::parse_program(&ast, input) {
        Ok(prog) => prog,
        Err(errors) => {
            let error = parse::SourceErrors {
                code: NamedSource::new("input", input.to_string()),
                errors,
            };

            let mut buf = String::with_capacity(1024);
            GraphicalReportHandler::new()
                .render_report(&mut buf, &error)
                .unwrap();

            println!("{buf}");
            return;
        }
    };

    let mut hir = hir::Hir {
        type_functions: &Arena::new(),
        expr_appls: &Arena::new(),
        list_expr_arms: &Arena::new(),
        list_exprs: &Arena::new(),
        list_types: &Arena::new(),
        list_pats: &Arena::new(),
        typevars: Vec::new(),
        equations: hir::Equations::new(),
    };

    // temporary for now until we can have custom named types
    let type_scope: HashMap<&str, hir::Type<'_>> = HashMap::new();

    let mut function_to_typescope: HashMap<&str, HashMap<&str, hir::Type<'_>>> = HashMap::new();

    let globals: HashMap<&str, hir::Polytype> = hir
        .default_globals()
        .chain(program.items.iter().map(|item| {
            // Since items (i.e. functions for now) can be generic over types,
            // we need to extend the set of currently in-scope types with the
            // generics that this item introduces. To avoid bringing the types
            // into the global program scope, we'll create a temporary inner scope
            let mut inner_type_scope = type_scope.clone();

            let mut typevars = SmallVec::with_capacity(item.generics.len());
            inner_type_scope.reserve(item.generics.len());

            for generic in item.generics.iter() {
                let var = hir.new_typevar();
                typevars.push(var);
                inner_type_scope.insert(
                    generic.literal,
                    hir::Type {
                        kind: hir::TypeKind::Var(var),
                        span: generic.span(),
                    },
                );
            }

            let typ = hir.type_from_ast(item.typ, &inner_type_scope);

            function_to_typescope.insert(item.name.literal, inner_type_scope);

            (item.name.literal, hir::Polytype { typevars, ty: typ })
        }))
        .collect();

    let mut locals: Vec<(&str, hir::Type<'_>)> = Vec::with_capacity(16);
    let mut errors: Vec<hir::LowerError<'_>> = Vec::with_capacity(0);

    let lowered_items: Result<
        HashMap<&str, (hir::Polytype, hir::Expr<'_, '_>)>,
        hir::PushedErrors,
    > = program
        .items
        .iter()
        .map(|item| {
            let item_name = item.name.literal;
            let mut scope = hir::Scope::new(
                &mut hir,
                &function_to_typescope[item_name],
                &mut errors,
                &globals,
                &mut locals,
            );

            let polytype = globals[item_name].clone();
            let expr = scope.lower(item.expr)?;

            // Make sure the function actually lines up with its type signature.
            let ty = scope.hir.monomorphize(&polytype);
            scope.unify(expr.ty(), ty);
            if scope.had_errors() {
                return Err(hir::PushedErrors);
            }
            Ok((item_name, (polytype, expr)))
        })
        .collect();

    // Put the result into: https://edotor.net/
    // println!("{}", hir.equations);

    let Ok(lowered_items) = lowered_items else {
        assert!(!errors.is_empty());
        let errors = hir::SourceErrors {
            code: NamedSource::new("input", input.to_string()),
            errors,
        };
        let mut buf = String::with_capacity(1024);
        GraphicalReportHandler::new()
            .render_report(&mut buf, &errors)
            .unwrap();
        println!("{buf}");
        return;
    };

    assert!(errors.is_empty());
    // let mut builder = dot::Builder::new(&hir);
    // for (name, (_, expr)) in lowered_items.iter() {
    //     builder.visit_expr(*expr, None, Some(name));
    // }
    // let out = builder.finish();
    // println!("{out}");

    let mut reports = vec![];
    for (_, expr) in lowered_items.values() {
        hir::usefulness::check_matches_in_expr(expr, &hir, &mut reports);
    }

    if reports.is_empty() {
        println!("all matches are exhaustive and useful");
    } else {
        println!("{reports:#?}");
    }
}
