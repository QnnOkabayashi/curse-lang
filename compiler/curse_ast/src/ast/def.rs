use crate::ast::{tok, Closure, Iter, TypeRef};
use crate::ast_struct;
use curse_span::HasSpan;

/// Example: `(K * V)`
///
/// We do this instead of having actual tuples because otherwise
/// `Collection (I32, I32)` would be ambiguous for `Collection<i32, i32>`
/// and `Collection<(i32, i32)>`.
#[derive(Clone, Debug)]
pub enum GenericParams<'ast> {
    Single(tok::Literal<'ast>),
    CartesianProduct(
        tok::LParen,
        Vec<(tok::Literal<'ast>, tok::Star)>,
        tok::Literal<'ast>,
        tok::RParen,
    ),
}

impl<'ast> GenericParams<'ast> {
    pub fn iter_params(&self) -> Iter<'_, tok::Literal<'ast>, tok::Star> {
        let (slice, last) = match self {
            GenericParams::Single(last) => (&[] as _, Some(last)),
            GenericParams::CartesianProduct(_, vec, last, _) => (vec.as_slice(), Some(last)),
        };

        Iter::new(slice.iter(), last)
    }
}

ast_struct! {
    /// Example: `fn add = |x, y| x + y`
    #[derive(Clone, Debug)]
    pub struct FunctionDef<'ast> {
        pub fn_: tok::Fn,
        pub ident: tok::Literal<'ast>,
        pub explicit_types: Option<ExplicitTypes<'ast>>,
        pub eq: tok::Eq,
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
    /// Example: `struct Id = I32`
    #[derive(Clone, Debug)]
    pub struct StructDef<'ast> {
        pub struct_: tok::Struct,
        pub ident: tok::Literal<'ast>,
        pub generic_params: Option<GenericParams<'ast>>,
        pub eq: tok::Eq,
        pub ty: TypeRef<'ast>,
    }
}

ast_struct! {
    /// Example: `choice Option T = Some T | None {}`
    #[derive(Clone, Debug)]
    pub struct ChoiceDef<'ast> {
        pub choice: tok::Choice,
        pub ident: tok::Literal<'ast>,
        pub generic_params: Option<GenericParams<'ast>>,
        pub eq: tok::Eq,
        pub variants: Variants<'ast>,
    }
}

#[derive(Debug, Clone)]
pub enum Variants<'ast> {
    /// "|"
    Never(tok::Pipe),
    /// "|"? (VariantDef "|")* VariantDef
    Variants(
        Option<tok::Pipe>,
        Vec<(VariantDef<'ast>, tok::Pipe)>,
        VariantDef<'ast>,
    ),
}

impl<'ast> Variants<'ast> {
    pub fn iter_variants(&self) -> Iter<'_, VariantDef<'ast>, tok::Pipe> {
        let (slice, last) = match self {
            Variants::Never(_) => (&[] as _, None),
            Variants::Variants(_, vec, last) => (vec.as_slice(), Some(last)),
        };

        Iter::new(slice.iter(), last)
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

impl HasSpan for VariantDef<'_> {
    fn start(&self) -> u32 {
        self.ident.start()
    }

    fn end(&self) -> u32 {
        self.ty.end()
    }
}
