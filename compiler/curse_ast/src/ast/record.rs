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
        pub fields: Vec<(Field<Value>, tok::Comma)>,
        pub last: Option<Field<Value>>,
        pub rbrace: tok::RBrace,
    }
}

#[derive(Clone, Debug)]
pub enum Field<Value> {
    Shorthand(Ident),
    BindingAndValue {
        binding: FieldBinding,
        colon: tok::Colon,
        value: Value,
    },
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

#[derive(Clone, Debug)]
pub struct BindingTree {
    pub lbrace: tok::LBrace,
    pub bindings: Vec<(Ident, tok::Colon, Option<FieldBinding>)>,
    pub rbrace: tok::RBrace,
}

impl<Value> Record<Value> {
    pub fn iter_fields(&self) -> Iter<'_, Field<Value>, tok::Comma> {
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

impl<T: HasSpan> HasSpan for Field<T> {
    fn start(&self) -> u32 {
        match self {
            Field::Shorthand(ident) => ident.start(),
            Field::BindingAndValue { binding, .. } => binding.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Field::Shorthand(ident) => ident.end(),
            Field::BindingAndValue { value, .. } => value.end(),
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

