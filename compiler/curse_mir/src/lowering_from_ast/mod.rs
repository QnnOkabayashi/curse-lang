use crate::{
    ctx,
    defs::{ChoiceDef, ChoiceDefId, StructDef, StructDefId},
    TypeSymbol,
};
use curse_ast as ast;
use std::collections::HashMap;

fn lower_defs(program: &ast::Program<'_, '_>, ctx: &ctx::Global<'_>) {
    let lowered_struct_defs: Vec<StructDef> = Vec::with_capacity(program.structs.len());
    let lowered_choice_defs: Vec<ChoiceDef> = Vec::with_capacity(program.choices.len());

    let struct_name_to_id: HashMap<TypeSymbol, StructDefId> = program
        .structs
        .iter()
        .enumerate()
        .map(|(index, struct_def)| {
            let symbol = ctx.get_or_intern_type_ident(&struct_def.name).symbol;
            (symbol, StructDefId(index))
        })
        .collect();

    let choice_name_to_id: HashMap<TypeSymbol, ChoiceDefId> = program
        .choices
        .iter()
        .enumerate()
        .map(|(index, choice_def)| {
            let symbol = ctx.get_or_intern_type_ident(&choice_def.name).symbol;
            (symbol, ChoiceDefId(index))
        })
        .collect();

    // let struct_defs = struct_name_to_id.into_iter().map(|(symbol, id)| (symbol, lowered_struct_defs))

    for (index, struct_def) in program.structs.iter().enumerate() {
        let name = ctx.get_or_intern_type_ident(&struct_def.name);
        let generics = ctx.type_idents.alloc_extend(
            struct_def
                .generic_params
                .iter()
                .map(|generic| ctx.get_or_intern_type_ident(generic)),
        );
        let fields = match &struct_def.inner {
            ast::FieldsKind::Newtype(ty) => {
                match ty {
                    ast::Type::Named(named) => {
                        let _x = ctx.get_or_intern_type_ident(&named.name);
                    }
                    ast::Type::Tuple(_) => todo!(),
                    ast::Type::Error => todo!(),
                };

                todo!()
                // FieldKind::Newtype(...);
            }
            ast::FieldsKind::Record(fields) => todo!(),
        };
    }
}

// take in a slice of struct and choice defs and convert them
// For now: no nested scope.
