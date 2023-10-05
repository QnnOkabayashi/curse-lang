use crate::ast::{tok, Closure, Type};
use crate::ast_struct;
use curse_interner::Ident;
use curse_span::HasSpan;

ast_struct! {
    /// Example: `|K * V|`
    #[derive(Clone, Debug)]
    pub struct GenericParams {
        pub open: tok::Pipe,
        pub params: Vec<Ident>,
        pub close: tok::Pipe,
    }
}

ast_struct! {
    /// Example: `fn add = |x, y| x + y`
    #[derive(Clone, Debug)]
    pub struct FunctionDef {
        pub fn_: tok::Fn,
        pub ident: Ident,
        pub function: Closure,
    }
}

ast_struct! {
    /// Example: `T: T -> T`
    #[derive(Clone, Debug)]
    pub struct ExplicitTypes {
        pub generic_params: Option<GenericParams>,
        pub colon: tok::Colon,
        pub ty: Type,
    }
}

ast_struct! {
    /// Example: `struct Id I32`
    #[derive(Clone, Debug)]
    pub struct StructDef {
        pub struct_: tok::Struct,
        pub ident: Ident,
        pub generic_params: Option<GenericParams>,
        pub ty: Type,
    }
}

ast_struct! {
    /// Example: `choice Option |T| { Some T, None {} }`
    #[derive(Clone, Debug)]
    pub struct ChoiceDef {
        pub choice: tok::Choice,
        pub ident: Ident,
        pub generic_params: Option<GenericParams>,
        pub variants: Variants,
    }
}

ast_struct! {
    /// Example: `{ Some T, None {} }`
    #[derive(Debug, Clone)]
    pub struct Variants {
        pub lbrace: tok::LBrace,
        pub variants: Vec<VariantDef>,
        pub rbrace: tok::RBrace,
    }
}

ast_struct! {
    /// Example: `Some T`
    #[derive(Clone, Debug)]
    pub struct VariantDef {
        pub ident: Ident,
        pub ty: Type,
    }
}

impl HasSpan for GenericParams {
    fn start(&self) -> u32 {
        self.open.start()
    }

    fn end(&self) -> u32 {
        self.close.end()
    }
}

impl HasSpan for FunctionDef {
    fn start(&self) -> u32 {
        self.fn_.start()
    }

    fn end(&self) -> u32 {
        self.function.end()
    }
}

impl HasSpan for StructDef {
    fn start(&self) -> u32 {
        self.struct_.start()
    }

    fn end(&self) -> u32 {
        self.ty.end()
    }
}

impl HasSpan for ChoiceDef {
    fn start(&self) -> u32 {
        self.choice.start()
    }

    fn end(&self) -> u32 {
        self.variants.end()
    }
}

impl HasSpan for Variants {
    fn start(&self) -> u32 {
        self.lbrace.start()
    }

    fn end(&self) -> u32 {
        self.rbrace.end()
    }
}

impl HasSpan for VariantDef {
    fn start(&self) -> u32 {
        self.ident.start()
    }

    fn end(&self) -> u32 {
        self.ty.end()
    }
}
