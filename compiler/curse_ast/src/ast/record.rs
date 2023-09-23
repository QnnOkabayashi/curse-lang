use crate::ast::{tok, Iter};
use crate::ast_struct;
use curse_interner::Ident;
use curse_span::HasSpan;

// Kind of places record syntax can show up:
// 1. types (binding tree: type)
// 2. value construction (binding tree: expression)
// 3. value destruction in a pattern match (name: pat)
//
// {
//     { x, y }: Point2D,
// }
//
// {
//     x: Point2D.x
// }

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Record<Value> {
        pub lbrace: tok::LBrace,
        pub fields: Vec<(FieldSyntax<Value>, tok::Comma)>,
        pub last: Option<FieldSyntax<Value>>,
        pub rbrace: tok::RBrace,
    }
}

#[derive(Clone, Debug)]
pub enum FieldSyntax<Value> {
    Shorthand(Ident),
    BindingAndValue(FieldBinding, tok::Colon, Value),
}

#[derive(Clone, Debug)]
pub enum FieldBinding {
    Binding(Ident),
    Tree(
        tok::LBrace,
        Vec<(Ident, Option<(tok::Colon, FieldBinding)>)>,
        tok::RBrace,
    ),
}

impl<Value> Record<Value> {
    pub fn iter_fields(&self) -> Iter<'_, FieldSyntax<Value>, tok::Comma> {
        Iter::new(self.fields.iter(), self.last.as_ref())
    }
}

impl<T> HasSpan for Record<T> {
    fn start(&self) -> u32 {
        self.lbrace.start()
    }

    fn end(&self) -> u32 {
        self.rbrace.end()
    }
}

impl<T: HasSpan> HasSpan for FieldSyntax<T> {
    fn start(&self) -> u32 {
        match self {
            FieldSyntax::Shorthand(ident) => ident.start(),
            FieldSyntax::BindingAndValue(binding, _, _) => binding.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            FieldSyntax::Shorthand(ident) => ident.end(),
            FieldSyntax::BindingAndValue(_, _, value) => value.end(),
        }
    }
}

impl HasSpan for FieldBinding {
    fn start(&self) -> u32 {
        match self {
            FieldBinding::Binding(binding) => binding.start(),
            FieldBinding::Tree(lbrace, _, _) => lbrace.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            FieldBinding::Binding(binding) => binding.end(),
            FieldBinding::Tree(_, _, rbrace) => rbrace.end(),
        }
    }
}
