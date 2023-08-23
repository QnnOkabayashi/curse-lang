use crate::ast::{tok, Iter};
use crate::ast_struct;
use curse_span::HasSpan;

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Record<'ast, T> {
        pub lbrace: tok::LBrace,
        pub fields: Vec<(Field<'ast, T>, tok::Comma)>,
        pub trailing: Option<Field<'ast, T>>,
        pub rbrace: tok::RBrace,
    }
}

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Field<'ast, T> {
        pub ident: tok::Literal<'ast>,
        pub value: Option<(tok::Colon, T)>,
    }
}

impl<'ast, T> Record<'ast, T> {
    pub fn iter_fields(&self) -> Iter<'_, Field<'ast, T>, tok::Comma> {
        Iter::new(self.fields.iter(), self.trailing.as_ref())
    }
}

impl<T> HasSpan for Record<'_, T> {
    fn start(&self) -> u32 {
        self.lbrace.start()
    }

    fn end(&self) -> u32 {
        self.rbrace.end()
    }
}

impl<T: HasSpan> HasSpan for Field<'_, T> {
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
