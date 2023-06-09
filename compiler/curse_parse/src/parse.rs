use crate::{Arena, ArenaAlloc};
use curse_ast::{tok, Field, Record};

/// Example: "{ score: A, complete: B }"
pub fn record<T>(
    lbrace: tok::LBrace,
    fields: Vec<(T, tok::Comma)>,
    trailing: Option<T>,
    rbrace: tok::RBrace,
) -> Record<T> {
    Record {
        lbrace,
        fields,
        trailing,
        rbrace,
    }
}

pub mod def {
    use crate::Arena;
    use curse_ast::{
        tok, ChoiceDef, Closure, ExplicitTypes, FieldsKind, FunctionDef, GenericParams, StructDef,
        Type, VariantDef, Variants,
    };

    pub fn generic_single<'input>(name: tok::TypeIdent<'input>) -> GenericParams<'input> {
        GenericParams::Single(name)
    }

    pub fn generic_product<'input>(
        lparen: tok::LParen,
        types: Vec<(tok::TypeIdent<'input>, tok::Star)>,
        trailing: tok::TypeIdent<'input>,
        rparen: tok::RParen,
    ) -> GenericParams<'input> {
        GenericParams::CartesianProduct {
            lparen,
            types,
            trailing,
            rparen,
        }
    }

    /// Example: `T: Option T`
    pub fn explicit_types<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        generics_decl: Option<GenericParams<'ast, 'input>>,
        colon: tok::Colon,
        ty: Type<'ast, 'input>,
    ) -> ExplicitTypes<'ast, 'input> {
        ExplicitTypes {
            generics,
            colon,
            ty: arena.types.alloc(ty),
        }
    }

    /// Example: `fn add = |a, b| a + b`
    pub fn function<'ast, 'input>(
        fn_: tok::Fn,
        name: tok::Ident<'input>,
        explicit_types: Option<ExplicitTypes<'ast, 'input>>,
        eq: tok::Eq,
        function: Closure<'ast, 'input>,
    ) -> FunctionDef<'ast, 'input> {
        FunctionDef {
            fn_,
            name,
            explicit_types,
            eq,
            function,
        }
    }

    /// Example: `struct Point T = { x: T, y: T }`
    pub fn struct_<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        struct_: tok::Struct,
        name: tok::TypeIdent<'input>,
        generic_params: Option<GenericParams<'ast, 'input>>,
        eq: tok::Eq,
        inner: Type<'ast, 'input>,
    ) -> StructDef<'ast, 'input> {
        StructDef {
            struct_,
            name,
            generic_params,
            eq,
            inner: arena.types.alloc(inner),
        }
    }

    /// Example: `choice Option T = Some T | None {}`
    pub fn choice<'ast, 'input>(
        choice: tok::Choice,
        name: tok::TypeIdent<'input>,
        generic_params: Option<GenericParams<'ast, 'input>>,
        eq: tok::Eq,
        variants: Variants<'ast, 'input>,
    ) -> ChoiceDef<'ast, 'input> {
        ChoiceDef {
            choice,
            name,
            eq,
            generic_params,
            variants,
        }
    }

    /// Example: `Some T | None {}`
    pub fn variants<'ast, 'input>(
        pipe: Option<tok::Pipe>,
        variants: Vec<(VariantDef<'ast, 'input>, tok::Pipe)>,
        trailing: VariantDef<'ast, 'input>,
    ) -> Variants<'ast, 'input> {
        Variants::Variants {
            pipe,
            trailing,
            variants,
        }
    }

    /// Example: `Some I32`
    pub fn variant<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        name: tok::TypeIdent<'input>,
        inner: Type<'ast, 'input>,
    ) -> VariantDef<'ast, 'input> {
        VariantDef {
            name,
            inner: arena.types.alloc(inner),
        }
    }
}

pub mod ty {
    use crate::Arena;
    use curse_ast::{tok, FieldType, GenericArgs, NamedType, Type};

    /// Example: `I32`
    pub fn named<'ast, 'input>(
        name: tok::TypeIdent<'input>,
        generic_args: Option<GenericArgs<'input>>,
    ) -> NamedType<'ast, 'input> {
        NamedType { name, generic_args }
    }

    /// Example: `Option I32`
    pub fn generic_single<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        ty: Type<'ast, 'input>,
    ) -> GenericArgs<'input> {
        GenericArgs::Single(arena.types.alloc(ty))
    }

    /// Example: `Result { I32, Error }`
    pub fn generic_product<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        lparen: tok::LParen,
        types: Vec<(Type<'ast, 'input>, tok::Star)>,
        trailing: Type<'ast, 'input>,
        rparen: tok::RParen,
    ) -> NamedType<'ast, 'input> {
        GenericArgs::CartesianProduct {
            lparen,
            types,
            trailing: arena.types.alloc(trailing),
            rparen,
        }
    }

    pub fn field<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        name: tok::Ident<'input>,
        colon: tok::Colon,
        ty: Type<'ast, 'input>,
    ) -> FieldType<'ast, 'input> {
        FieldType {
            name,
            colon,
            ty: arena.types.alloc(ty),
        }
    }
}

pub mod pat {
    use curse_ast::{FieldPat, Pat};
    // TODO(quinn) add choice and struct destructuring patterns

    pub fn field<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        name: tok::Ident<'input>,
        explicit_value: Option<(tok::Colon, Pat<'ast, 'input>)>,
    ) -> FieldPat<'ast, 'input> {
        FieldPat {
            name,
            explicit_value: explicit_value.map(|(colon, pat)| (colon, arena.pats.alloc(pat))),
        }
    }
}

pub mod expr {
    use crate::Arena;
    use curse_ast::{
        tok, Appl, Arm, Closure, Constructor, Expr, Field, FieldsKind, Param, Paren, Pat, Type,
    };

    pub fn field<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        name: tok::Ident<'input>,
        explicit_value: Option<(tok::Colon, Expr<'ast, 'input>)>,
    ) -> FieldExpr<'ast, 'input> {
        FieldExpr {
            name,
            explicit_value: explicit_value.map(|(colon, expr)| (colon, arena.exprs.alloc(expr))),
        }
    }

    /// Example: "(|true| 1, |false| 0,)"
    pub fn closure_piecewise<'ast, 'input>(
        lparen: tok::LParen,
        arms: Vec<(Arm<'ast, 'input>, tok::Comma)>,
        rparen: tok::RParen,
    ) -> Closure<'ast, 'input> {
        Closure::Piecewise {
            lparen,
            arms,
            rparen,
        }
    }

    /// Example: "|x: I32| x + 1"
    pub fn closure_arm<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        open: tok::Pipe,
        params: Vec<(Param<'ast, 'input>, tok::Comma)>,
        trailing: Option<Param<'ast, 'input>>,
        close: tok::Pipe,
        body: Expr<'ast, 'input>,
    ) -> Arm<'ast, 'input> {
        Arm {
            open,
            params,
            trailing,
            close,
            body: arena.exprs.alloc(body),
        }
    }

    /// Example: "x: I32"
    pub fn closure_param<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        pat: Pat<'ast, 'input>,
        ascription: Option<(tok::Colon, Type<'ast, 'input>)>,
    ) -> Param<'ast, 'input> {
        Param {
            pat: arena.pats.alloc(pat),
            ascription: ascription.map(|(colon, ty)| (colon, &*arena.types.alloc(ty))),
        }
    }

    /// Example: "Ok true"
    pub fn constructor<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        name: tok::TypeIdent<'input>,
        expr: Expr<'ast, 'input>,
    ) -> Constructor<'ast, 'input> {
        Constructor {
            name,
            inner: arena.exprs.alloc(expr),
        }
    }
    /// Example: "list map |x| x + 1"
    pub fn appl<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        lhs: Expr<'ast, 'input>,
        fun: Expr<'ast, 'input>,
        rhs: Expr<'ast, 'input>,
    ) -> Appl<'ast, 'input> {
        Appl {
            lhs: arena.exprs.alloc(lhs),
            fun: arena.exprs.alloc(fun),
            rhs: arena.exprs.alloc(rhs),
        }
    }

    /// Example: "(a + b)"
    pub fn paren<'ast, 'input>(
        arena: &'ast Arena<'ast, 'input>,
        lparen: tok::LParen,
        expr: Expr<'ast, 'input>,
        rparen: tok::RParen,
    ) -> Paren<'ast, 'input> {
        Paren {
            lparen,
            expr: arena.exprs.alloc(expr),
            rparen,
        }
    }
}
