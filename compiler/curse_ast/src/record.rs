use crate::{tok, Span};

#[derive(Clone, Debug)]
pub struct Record<T> {
    pub lbrace: tok::LBrace,
    pub fields: Vec<(T, tok::Comma)>,
    pub trailing: Option<T>,
    pub rbrace: tok::RBrace,
}

impl<T> Span for Record<T> {
    fn start(&self) -> usize {
        self.lbrace.start()
    }

    fn end(&self) -> usize {
        self.rbrace.end()
    }
}
