use curse_ast::Span;
use curse_hir::{self as hir, ctx};
use curse_parse as parse;
use hir::Ty;
use miette::{GraphicalReportHandler, NamedSource};
use smallvec::SmallVec;
use std::collections::HashMap;

mod programs;

// TODO(quinn):
// Make it so that tuples don't count as possible functions
// Choice variant usefulness
// Choice variant parsing in expressions and patterns
// type-inferred function syntax
// Attributes (like inline)
// Tail recursion checking- I feel like with tail recursion, people either _need_ it or do not care
// about it. It shouldn't be a hassle to determine if it's applied or not, so we should have a
// `#[tail_recursive]` attribute where if present, it _will_ be tail recursive (or compiler error),
// and if absent, no tail recursion will be applied. This would require good diagnostics for why it
// cannot be applied too if that error occurs.
// Since this language is pretty functional right now, tail recursion is a must.

fn main() {
    let input: &str = programs::FIB;

    let ast_arena = parse::Arena::default();
    let program = match parse::parse_program(&ast_arena, input) {
        Ok(program) => program,
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

    //
    // // println!("{:#?}", program.choice_defs);
    //
    // let global_ctx = ctx::Global::default();
    //
    // let mut typeck_ctx = ctx::Typeck::with_global(&global_ctx);
    //
    // // Once we have custom types, we'll need to add them here
    // let type_scope: HashMap<hir::TypeSymbol, hir::Type<'_>> = HashMap::new();
    //
    // // program.struct_defs
    // // program.choice_defs
    //
    // let mut function_to_typescope: HashMap<
    //     hir::ValueSymbol,
    //     HashMap<hir::TypeSymbol, hir::Type<'_>>,
    // > = HashMap::new();
    //
    // let globals: HashMap<hir::ValueSymbol, hir::TypeTemplate> = typeck_ctx
    //     .default_globals()
    //     .chain(program.fn_defs.iter().map(|fn_def| {
    //         // Since items (i.e. functions for now) can be generic over types,
    //         // we need to extend the set of currently in-scope types with the
    //         // generics that this item introduces. To avoid bringing the types
    //         // into the global program scope, we'll create a temporary inner scope
    //         let mut inner_type_scope = type_scope.clone();
    //
    //         let sig = fn_def
    //             .type_sig
    //             .as_ref()
    //             .expect("functions with inferred type signatures aren't yet supported");
    //
    //         let mut typevars = SmallVec::with_capacity(sig.generics.len());
    //         inner_type_scope.reserve(sig.generics.len());
    //
    //         for generic in sig.generics.iter() {
    //             let var = typeck_ctx.new_typevar();
    //             typevars.push(var);
    //             inner_type_scope.insert(
    //                 typeck_ctx.global.get_or_intern_type_ident(generic).symbol,
    //                 hir::Type {
    //                     kind: hir::TypeKind::Var(var),
    //                     span: generic.span(),
    //                 },
    //             );
    //         }
    //
    //         // TODO(quinn): make types brought into scope for the particular function
    //         // (aka the generics here) be passed in in some other intelligent
    //         // way so we don't have to clone the hashmap.
    //         let ty = typeck_ctx.type_from_ast(sig.ty, &inner_type_scope);
    //
    //         let fn_name = typeck_ctx
    //             .global
    //             .get_or_intern_value_ident(&fn_def.name)
    //             .symbol;
    //
    //         function_to_typescope.insert(fn_name, inner_type_scope);
    //         (fn_name, hir::TypeTemplate { typevars, ty })
    //     }))
    //     .collect();
    //
    // let mut locals: Vec<(hir::ValueIdent, hir::Type<'_>)> = Vec::with_capacity(16);
    // let mut errors: Vec<hir::LowerError<'_>> = Vec::with_capacity(0);
    //
    // let lowered_items: Result<
    //     HashMap<hir::ValueSymbol, (hir::TypeTemplate, hir::Expr<'_>)>,
    //     hir::PushedErrors,
    // > = program
    //     .fn_defs
    //     .iter()
    //     .map(|item| {
    //         let item_name: hir::ValueSymbol = typeck_ctx
    //             .global
    //             .get_or_intern_value_ident(&item.name)
    //             .symbol;
    //
    //         let mut scope = hir::Scope::new(
    //             &mut typeck_ctx,
    //             &function_to_typescope[&item_name],
    //             &mut errors,
    //             &globals,
    //             &mut locals,
    //         );
    //
    //         let polytype = globals[&item_name].clone();
    //         let expr = scope.lower_closure(&item.function)?;
    //
    //         // Make sure the function actually lines up with its type signature.
    //         let ty = scope.ctx.monomorphize(&polytype);
    //         scope.unify(expr.ty(), ty);
    //         if scope.had_errors() {
    //             return Err(hir::PushedErrors);
    //         }
    //         Ok((item_name, (polytype, expr)))
    //     })
    //     .collect();
    //
    // // Lowering and type errors
    // let Ok(lowered_items) = lowered_items else {
    //     assert!(!errors.is_empty());
    //     let errors = hir::LowerErrors {
    //         code: NamedSource::new("input", input.to_string()),
    //         errors,
    //     };
    //     let mut buf = String::with_capacity(1024);
    //     GraphicalReportHandler::new()
    //         .render_report(&mut buf, &errors)
    //         .unwrap();
    //     println!("{buf}");
    //     return;
    // };
    //
    // assert!(errors.is_empty());
    //
    // let usefulness_errors =
    //     hir::usefulness::check(lowered_items.values().map(|(_, expr)| expr), &typeck_ctx);
    //
    // if usefulness_errors.is_empty() {
    //     println!("all matches are exhaustive and useful");
    // } else {
    //     let errors = hir::usefulness::UsefulnessErrors {
    //         code: NamedSource::new("input", input.to_string()),
    //         errors: usefulness_errors,
    //     };
    //     let mut buf = String::with_capacity(1024);
    //     GraphicalReportHandler::new()
    //         .render_report(&mut buf, &errors)
    //         .unwrap();
    //     println!("{buf}");
    //     return;
    // }
}
