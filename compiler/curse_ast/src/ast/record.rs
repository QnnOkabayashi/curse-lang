use crate::ast::{tok, Iter};
use crate::ast_struct;
use curse_span::HasSpan;
use curse_interner::Ident;

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Record<T> {
        pub lbrace: tok::LBrace,
        pub fields: Vec<(Field<T>, tok::Comma)>,
        pub trailing: Option<Field<T>>,
        pub rbrace: tok::RBrace,
    }
}

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Field<T> {
        pub ident: Ident,
        pub value: Option<(tok::Colon, T)>,
    }
}

impl<T> Record<T> {
    pub fn iter_fields(&self) -> Iter<'_, Field<T>, tok::Comma> {
        Iter::new(self.fields.iter(), self.trailing.as_ref())
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
        self.ident.start()
    }

    fn end(&self) -> u32 {
        if let Some((_, value)) = self.value.as_ref() {
            value.end()
        } else {
            self.ident.end()
        }
    }
}
