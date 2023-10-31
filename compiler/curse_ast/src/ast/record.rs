use crate::ast::{tok, Pat};
use curse_span::HasSpan;
use derive_more::From;

#[derive(Clone, Debug, From)]
pub struct Record<T> {
    pub lbrace: tok::LBrace,
    pub fields: Vec<(Pat, Option<T>)>,
    pub rbrace: tok::RBrace,
}

impl<T> HasSpan for Record<T> {
    fn start(&self) -> u32 {
        self.lbrace.start()
    }

    fn end(&self) -> u32 {
        self.rbrace.end()
    }
}
