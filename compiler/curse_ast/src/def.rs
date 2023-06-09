use crate::{tok, ty::Type, Closure, Span};

/// Example: `(K * V)`
#[derive(Clone, Debug)]
pub enum GenericParams<'input> {
    Single(tok::TypeIdent<'input>),
    CartesianProduct {
        lparen: tok::LParen,
        types: Vec<(tok::TypeIdent<'input>, tok::Star)>,
        trailing: tok::TypeIdent<'input>,
        rparen: tok::RParen,
    },
}

/// Example: `fn add = |x, y| x + y`
#[derive(Clone, Debug)]
pub struct FunctionDef<'ast, 'input> {
    pub fn_: tok::Fn,
    pub name: tok::Ident<'input>,
    pub explicit_types: Option<ExplicitTypes<'ast, 'input>>,
    pub eq: tok::Eq,
    pub function: Closure<'ast, 'input>,
}

/// Example: `T: T -> T`
#[derive(Clone, Debug)]
pub struct ExplicitTypes<'ast, 'input> {
    pub generics: GenericParams<'input>,
    pub colon: tok::Colon,
    pub ty: &'ast Type<'ast, 'input>,
}

/// Example: `struct Id = I32`
#[derive(Clone, Debug)]
pub struct StructDef<'ast, 'input> {
    pub struct_: tok::Struct,
    pub name: tok::TypeIdent<'input>,
    pub generic_params: GenericParams<'input>,
    pub eq: tok::Eq,
    pub inner: &'ast Type<'ast, 'input>,
}

/// Example: `choice Option T = Some T | None {}`
#[derive(Clone, Debug)]
pub struct ChoiceDef<'ast, 'input> {
    pub choice: tok::Choice,
    pub name: tok::TypeIdent<'input>,
    pub generic_params: GenericParams<'input>,
    pub eq: tok::Eq,
    pub variants: Variants<'ast, 'input>,
}

#[derive(Debug, Clone)]
pub enum Variants<'ast, 'input> {
    /// "|"
    Never(tok::Pipe),
    /// "|"? (VariantDef "|")* VariantDef
    Variants {
        pipe: Option<tok::Pipe>,
        variants: Vec<(VariantDef<'ast, 'input>, tok::Pipe)>,
        trailing: VariantDef<'ast, 'input>,
    },
}

/// Example: `Some T`
#[derive(Clone, Debug)]
pub struct VariantDef<'ast, 'input> {
    pub name: tok::TypeIdent<'input>,
    pub inner: &'ast Type<'ast, 'input>,
}

impl Span for ChoiceDef<'_, '_> {
    fn start(&self) -> usize {
        self.choice.start()
    }

    fn end(&self) -> usize {
        self.variants.end()
    }
}

impl Span for Variants<'_, '_> {
    fn start(&self) -> usize {
        match self {
            Variants::Never(pipe) => pipe.start(),
            Variants::Variants {
                pipe,
                variants,
                trailing,
            } => match (pipe, variants.as_slice(), trailing) {
                (Some(pipe), _, _) => pipe.start(),
                (None, [(first, _), ..], _) => first.start(),
                (None, [], last) => last.start(),
            },
        }
    }

    fn end(&self) -> usize {
        match self {
            Variants::Never(pipe) => pipe.end(),
            Variants::Variants { trailing, .. } => trailing.end(),
        }
    }
}

impl Span for VariantDef<'_, '_> {
    fn start(&self) -> usize {
        self.name.start()
    }

    fn end(&self) -> usize {
        self.inner.end()
    }
}
