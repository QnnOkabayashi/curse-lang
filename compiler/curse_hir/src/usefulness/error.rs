//! Usefulness errors

use std::iter;

use crate::ExprArm;
use curse_ast::Span;
use miette::{Diagnostic, LabeledSpan, NamedSource};
use smallvec::SmallVec;
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
#[error("Errors in usefulness checking")]
pub struct UsefulnessErrors<'hir, 'input> {
    #[source_code]
    pub code: NamedSource,

    #[related]
    pub errors: Vec<UsefulnessError<'hir, 'input>>,
}

/// Report the usefulness of one particular piecewise function
///
/// Invariants: either redundent_arms is nonempty or is_exhaustive is true.
#[derive(Debug, Error)]
#[error("Usefulness error")]
pub struct UsefulnessError<'hir, 'input> {
    pub redundent_arms: Vec<RedundentArmError<'hir, 'input>>,
    /// The last branch that doesn't exhaust the rest (we need to highlight something!)
    pub non_exhaustive: Option<&'hir ExprArm<'hir, 'input>>,
}

/// A useless arm and its coverers, i.e. the arms above it that render
/// it useless.
#[derive(Debug, Error)]
#[error("Redundent arm in piecewise function")]
pub struct RedundentArmError<'hir, 'input> {
    /// Invariants: at least one coverer
    pub coverers: SmallVec<[&'hir ExprArm<'hir, 'input>; 1]>,
    pub redundent_arm: &'hir ExprArm<'hir, 'input>,
}

impl<'hir, 'input> Diagnostic for UsefulnessError<'hir, 'input> {
    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        let help = if self.non_exhaustive.is_none() {
            "Try resolving any redundent arm conflicts."
        } else if self.redundent_arms.is_empty() {
            "Try making the piecewise exhaustive."
        } else {
            "Try making the piecewise exhaustive and resolving any redundent arm conflicts."
        };

        Some(Box::new(help))
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let non_exhaustive = self.non_exhaustive?;

        Some(Box::new(iter::once(LabeledSpan::new_with_span(
            Some("This last branch doesn't exhaust all possible patterns".to_string()),
            non_exhaustive.span(),
        ))))
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        Some(Box::new(
            self.redundent_arms.iter().map(|x| x as &dyn Diagnostic),
        ))
    }
}

impl<'hir, 'input> Diagnostic for RedundentArmError<'hir, 'input> {
    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(
            "Try removing the redundent arm or using a different pattern.",
        ))
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(
            self.coverers
                .iter()
                .map(|coverer| {
                    LabeledSpan::new_with_span(Some("This arm...".to_string()), coverer.span())
                })
                .chain(Some(LabeledSpan::new_with_span(
                    Some("... makes this arm redundent.".to_string()),
                    self.redundent_arm.span(),
                ))),
        ))
    }
}
