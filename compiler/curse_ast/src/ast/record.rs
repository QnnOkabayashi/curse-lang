use crate::ast::tok;
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
    pub struct Record<T> {
        pub lbrace: tok::LBrace,
        pub fields: Vec<(Binding, Option<T>)>,
        pub rbrace: tok::RBrace,
    }
}

#[derive(Clone, Debug)]
pub enum Binding {
    // e.g. `foo`
    Ident(Ident),
    // e.g. `{ foo, bar }`
    Record(Record<Self>),
}

impl<T> HasSpan for Record<T> {
    fn start(&self) -> u32 {
        self.lbrace.start()
    }

    fn end(&self) -> u32 {
        self.rbrace.end()
    }
}

impl HasSpan for Binding {
    fn start(&self) -> u32 {
        match self {
            Binding::Ident(ident) => ident.start(),
            Binding::Record(record) => record.start(),
        }
    }

    fn end(&self) -> u32 {
        match self {
            Binding::Ident(ident) => ident.end(),
            Binding::Record(record) => record.end(),
        }
    }
}
