use crate::{ast_struct, tok, ty::Type, Closure};
use curse_span::HasSpan;

/// Example: `(K * V)`
#[derive(Clone, Debug)]
pub enum GenericParams<'input> {
    Single(tok::TypeIdent<'input>),
    CartesianProduct(
        tok::LParen,
        Vec<(tok::TypeIdent<'input>, tok::Star)>,
        tok::TypeIdent<'input>,
        tok::RParen,
    ),
}

ast_struct! {
    /// Example: `fn add = |x, y| x + y`
    #[derive(Clone, Debug)]
    pub struct FunctionDef<'ast, 'input> {
        pub fn_: tok::Fn,
        pub ident: tok::Ident<'input>,
        pub explicit_types: Option<ExplicitTypes<'ast, 'input>>,
        pub eq: tok::Eq,
        pub function: Closure<'ast, 'input>,
    }
}

ast_struct! {
    /// Example: `T: T -> T`
    #[derive(Clone, Debug)]
    pub struct ExplicitTypes<'ast, 'input> {
        pub generic_params: Option<GenericParams<'input>>,
        pub colon: tok::Colon,
        pub ty: &'ast Type<'ast, 'input>,
    }
}

ast_struct! {
    /// Example: `struct Id = I32`
    #[derive(Clone, Debug)]
    pub struct StructDef<'ast, 'input> {
        pub struct_: tok::Struct,
        pub ident: tok::TypeIdent<'input>,
        pub generic_params: Option<GenericParams<'input>>,
        pub eq: tok::Eq,
        pub inner: &'ast Type<'ast, 'input>,
    }
}

ast_struct! {
    /// Example: `choice Option T = Some T | None {}`
    #[derive(Clone, Debug)]
    pub struct ChoiceDef<'ast, 'input> {
        pub choice: tok::Choice,
        pub ident: tok::TypeIdent<'input>,
        pub generic_params: Option<GenericParams<'input>>,
        pub eq: tok::Eq,
        pub variants: Variants<'ast, 'input>,
    }
}

#[derive(Debug, Clone)]
pub enum Variants<'ast, 'input> {
    /// "|"
    Never(tok::Pipe),
    /// "|"? (VariantDef "|")* VariantDef
    Variants(
        Option<tok::Pipe>,
        Vec<(VariantDef<'ast, 'input>, tok::Pipe)>,
        VariantDef<'ast, 'input>,
    ),
}

ast_struct! {
    /// Example: `Some T`
    #[derive(Clone, Debug)]
    pub struct VariantDef<'ast, 'input> {
        pub ident: tok::TypeIdent<'input>,
        pub inner: &'ast Type<'ast, 'input>,
    }
}

impl HasSpan for FunctionDef<'_, '_> {
    fn start(&self) -> u32 {
        self.fn_.start()
    }

    fn end(&self) -> u32 {
        self.function.end()
    }
}

impl HasSpan for StructDef<'_, '_> {
    fn start(&self) -> u32 {
        self.struct_.start()
    }

    fn end(&self) -> u32 {
        self.inner.end()
    }
}

impl HasSpan for ChoiceDef<'_, '_> {
    fn start(&self) -> u32 {
        self.choice.start()
    }

    fn end(&self) -> u32 {
        self.variants.end()
    }
}

impl HasSpan for Variants<'_, '_> {
    fn start(&self) -> u32 {
        match self {
            Variants::Never(pipe) => pipe.start(),
            Variants::Variants(pipe, variants, last) => match (pipe, variants.as_slice(), last) {
                (Some(pipe), _, _) => pipe.start(),
                (None, [(first, _), ..], _) => first.start(),
                (None, [], last) => last.start(),
            },
        }
    }

    fn end(&self) -> u32 {
        match self {
            Variants::Never(pipe) => pipe.end(),
            Variants::Variants(.., last) => last.end(),
        }
    }
}

impl HasSpan for VariantDef<'_, '_> {
    fn start(&self) -> u32 {
        self.ident.start()
    }

    fn end(&self) -> u32 {
        self.inner.end()
    }
}
