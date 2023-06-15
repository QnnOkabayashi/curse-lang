use crate::{ast_struct, tok};
use curse_span::HasSpan;

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Record<'input, T> {
        pub lbrace: tok::LBrace,
        pub fields: Vec<(Field<'input, T>, tok::Comma)>,
        pub trailing: Option<Field<'input, T>>,
        pub rbrace: tok::RBrace,
    }
}

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Field<'input, T> {
        pub ident: tok::Ident<'input>,
        pub value: Option<(tok::Colon, T)>,
    }
}

impl<'input, T> Record<'input, T> {
    pub fn fields(&self) -> impl Iterator<Item = &Field<'input, T>> {
        self.fields
            .iter()
            .map(|(field, _comma)| field)
            .chain(self.trailing.as_ref())
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
