use crate::ast::{tok, Pat};
use crate::ast_struct;
use curse_span::HasSpan;

ast_struct! {
    #[derive(Clone, Debug)]
    pub struct Record<T> {
        pub lbrace: tok::LBrace,
        pub fields: Vec<(Pat, Option<T>)>,
        pub rbrace: tok::RBrace,
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
