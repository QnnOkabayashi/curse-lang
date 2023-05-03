use crate::{tok, Span, Type};

/// AST representation of a choice type definition.
#[derive(Debug, Clone)]
pub struct ChoiceDef<'ast, 'input> {
    pub choice: tok::Choice,
    pub ident: tok::Ident<'input>,
    pub generics: Vec<tok::Ident<'input>>,
    pub lbrace: tok::LBrace,
    pub variants: ChoiceVariants<'ast, 'input>,
    pub rbrace: tok::RBrace,
}

impl<'ast, 'input> ChoiceDef<'ast, 'input> {
    pub fn new(
        choice: tok::Choice,
        ident: tok::Ident<'input>,
        generics: Vec<tok::Ident<'input>>,
        lbrace: tok::LBrace,
        variants: ChoiceVariants<'ast, 'input>,
        rbrace: tok::RBrace,
    ) -> Self {
        ChoiceDef {
            choice,
            ident,
            generics,
            lbrace,
            variants,
            rbrace,
        }
    }
}

impl Span for ChoiceDef<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.choice.span_between(self.rbrace)
    }
}

#[derive(Debug, Clone)]
pub enum ChoiceVariants<'ast, 'input> {
    // {}
    Empty,
    // { v ("," v)* ","? }
    Variants {
        first: ChoiceVariant<'ast, 'input>,
        rest: Vec<(tok::Comma, ChoiceVariant<'ast, 'input>)>,
        trailing: Option<tok::Comma>,
    },
}

#[derive(Debug, Clone)]
pub struct ChoiceVariant<'ast, 'input> {
    pub apostrophe: tok::Apostrophe,
    pub tag: tok::Ident<'input>,
    pub payload: Option<&'ast Type<'ast, 'input>>,
}

impl<'ast, 'input> ChoiceVariant<'ast, 'input> {
    pub fn new(
        apostrophe: tok::Apostrophe,
        tag: tok::Ident<'input>,
        payload: Option<&'ast Type<'ast, 'input>>,
    ) -> Self {
        ChoiceVariant {
            apostrophe,
            tag,
            payload,
        }
    }
}
