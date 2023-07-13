use curse_hir::hir::PrimitiveType;
use curse_interner::{Ident, InternedString};
use curse_span::{HasSpan, Span};
use miette::{Diagnostic, LabeledSpan};
use std::{fmt, iter};

#[derive(Debug)]
pub enum LoweringError {
    TypeRecordMissingFieldType {
        field_ident: Ident,
    },
    IntegerLiteralOverflow {
        literal: String,
        span: Span,
    },
    TooManyClosureParams {
        /// at least 3
        all_params: Vec<Span>,
    },
    MultipleDefsWithSameName {
        ident: InternedString,
        previous: Span,
        redefined: Span,
    },
    UnexpectedTypeArgs {
        reason: UnexpectedTypeArgs,
        problem_type_span: Span,
        arg_spans: Vec<Span>,
    },
}

#[derive(Debug)]
pub enum UnexpectedTypeArgs {
    GenericParam { def_ident: Ident },
    Primitive(PrimitiveType),
}

impl std::error::Error for LoweringError {}

impl fmt::Display for LoweringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::TypeRecordMissingFieldType { .. } => {
                write!(f, "record types cannot omit the type of a field")
            }
            LoweringError::IntegerLiteralOverflow { .. } => {
                write!(
                    f,
                    "integer literal overflows a u32 (maximum is 4_294_967_295)"
                )
            }
            LoweringError::TooManyClosureParams { .. } => {
                write!(f, "too many parameters")
            }
            LoweringError::MultipleDefsWithSameName { ident, .. } => {
                write!(f, "the name `{ident}` is defined multiple times")
            }
            LoweringError::UnexpectedTypeArgs { reason, .. } => match reason {
                UnexpectedTypeArgs::GenericParam { def_ident } => write!(
                    f,
                    "type arguments are not allowed on type parameter `{def_ident}`"
                ),
                UnexpectedTypeArgs::Primitive(prim) => write!(
                    f,
                    "type arguments are not allowed on primitive type `{prim:?}`"
                ),
            },
        }
    }
}

impl Diagnostic for LoweringError {
    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        match self {
            LoweringError::TypeRecordMissingFieldType { field_ident } => Some(Box::new(format!(
                "add a type to this field, e.g. `{field_ident}: Type`"
            ))),
            LoweringError::IntegerLiteralOverflow { .. } => Some(Box::new("use a smaller value")),
            LoweringError::TooManyClosureParams { .. } => Some(Box::new(
                "if you need more than 2 arguments, try using a record",
            )),
            LoweringError::MultipleDefsWithSameName { ident, .. } => {
                Some(Box::new(format!("use a name other than `{ident}`")))
            }
            LoweringError::UnexpectedTypeArgs { .. } => Some(Box::new("remove the type arguments")),
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            LoweringError::TypeRecordMissingFieldType { field_ident } => {
                Some(Box::new(iter::once(LabeledSpan::at(
                    field_ident.span().start_len(),
                    format!("field `{field_ident}` is missing a type"),
                ))))
            }
            LoweringError::IntegerLiteralOverflow { literal, span } => Some(Box::new(iter::once(
                LabeledSpan::at(span.start_len(), format!("`{literal}` is too large")),
            ))),
            LoweringError::TooManyClosureParams { all_params } => {
                let start = all_params.get(2).expect("at least 3").start;
                let end = all_params.last().expect("at least 3").end;

                Some(Box::new(iter::once(LabeledSpan::at(
                    Span { start, end }.start_len(),
                    "there are too many parameters here",
                ))))
            }
            LoweringError::MultipleDefsWithSameName {
                ident,
                previous,
                redefined,
            } => Some(Box::new(
                [
                    LabeledSpan::at(
                        previous.start_len(),
                        format!("previous definition of `{ident}` here"),
                    ),
                    LabeledSpan::at(redefined.start_len(), format!("`{ident}` redefined here")),
                ]
                .into_iter(),
            )),
            LoweringError::UnexpectedTypeArgs {
                reason,
                problem_type_span: ty_span,
                arg_spans,
            } => {
                let arg_labels = arg_spans.iter().map(|arg_span| {
                    LabeledSpan::at(arg_span.start_len(), "type argument not allowed")
                });

                match reason {
                    UnexpectedTypeArgs::GenericParam { def_ident } => Some(Box::new(
                        [
                            LabeledSpan::at(
                                def_ident.span().start_len(),
                                format!("type parameter `{def_ident}` defined here"),
                            ),
                            LabeledSpan::at(
                                ty_span.start_len(),
                                format!("not allowed on type parameter `{def_ident}`"),
                            ),
                        ]
                        .into_iter()
                        .chain(arg_labels),
                    )),
                    UnexpectedTypeArgs::Primitive(prim) => Some(Box::new(
                        iter::once(LabeledSpan::at(
                            ty_span.start_len(),
                            format!("not allowed on primitive type `{prim:?}`"),
                        ))
                        .chain(arg_labels),
                    )),
                }
            }
        }
    }
}
