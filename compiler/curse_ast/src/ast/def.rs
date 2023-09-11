use crate::ast::{tok, Closure, Iter, TypeRef};
use crate::ast_struct;
use curse_span::HasSpan;

ast_struct! {
    /// Example: `|K * V|`
    #[derive(Clone, Debug)]
    pub struct GenericParams<'ast> {
        open: tok::Pipe,
        params: Vec<(tok::Literal<'ast>, tok::Star)>,
        last: tok::Literal<'ast>,
        close: tok::Pipe,
    }
}

impl<'ast> GenericParams<'ast> {
    pub fn iter_params(&self) -> Iter<'_, tok::Literal<'ast>, tok::Star> {
        Iter::new(self.params.iter(), Some(&self.last))
    }
}

ast_struct! {
    /// Example: `fn add = |x, y| x + y`
    #[derive(Clone, Debug)]
    pub struct FunctionDef<'ast> {
        pub fn_: tok::Fn,
        pub ident: tok::Literal<'ast>,
        pub function: Closure<'ast>,
    }
}

ast_struct! {
    /// Example: `T: T -> T`
    #[derive(Clone, Debug)]
    pub struct ExplicitTypes<'ast> {
        pub generic_params: Option<GenericParams<'ast>>,
        pub colon: tok::Colon,
        pub ty: TypeRef<'ast>,
    }
}

ast_struct! {
    /// Example: `struct Id I32`
    #[derive(Clone, Debug)]
    pub struct StructDef<'ast> {
        pub struct_: tok::Struct,
        pub ident: tok::Literal<'ast>,
        pub generic_params: Option<GenericParams<'ast>>,
        pub ty: TypeRef<'ast>,
    }
}

ast_struct! {
    /// Example: `choice Option |T| { Some T, None {} }`
    #[derive(Clone, Debug)]
    pub struct ChoiceDef<'ast> {
        pub choice: tok::Choice,
        pub ident: tok::Literal<'ast>,
        pub generic_params: Option<GenericParams<'ast>>,
        pub variants: Variants<'ast>,
    }
}

ast_struct! {
    /// Example: `{ Some T, None {} }`
    #[derive(Debug, Clone)]
    pub struct Variants<'ast> {
        lbrace: tok::LBrace,
        variants: Vec<(VariantDef<'ast>, tok::Comma)>,
        last: Option<VariantDef<'ast>>,
        rbrace: tok::RBrace,
    }
}

impl<'ast> Variants<'ast> {
    pub fn iter_variants(&self) -> Iter<'_, VariantDef<'ast>, tok::Comma> {
        Iter::new(self.variants.iter(), self.last.as_ref())
    }
}

ast_struct! {
    /// Example: `Some T`
    #[derive(Clone, Debug)]
    pub struct VariantDef<'ast> {
        pub ident: tok::Literal<'ast>,
        pub ty: TypeRef<'ast>,
    }
}

impl HasSpan for GenericParams<'_> {
    fn start(&self) -> u32 {
        self.open.start()
    }

    fn end(&self) -> u32 {
        self.close.end()
    }
}

impl HasSpan for FunctionDef<'_> {
    fn start(&self) -> u32 {
        self.fn_.start()
    }

    fn end(&self) -> u32 {
        self.function.end()
    }
}

impl HasSpan for StructDef<'_> {
    fn start(&self) -> u32 {
        self.struct_.start()
    }

    fn end(&self) -> u32 {
        self.ty.end()
    }
}

impl HasSpan for ChoiceDef<'_> {
    fn start(&self) -> u32 {
        self.choice.start()
    }

    fn end(&self) -> u32 {
        self.variants.end()
    }
}

impl HasSpan for Variants<'_> {
    fn start(&self) -> u32 {
        self.lbrace.start()
    }

    fn end(&self) -> u32 {
        self.rbrace.end()
    }
}

impl HasSpan for VariantDef<'_> {
    fn start(&self) -> u32 {
        self.ident.start()
    }

    fn end(&self) -> u32 {
        self.ty.end()
    }
}
