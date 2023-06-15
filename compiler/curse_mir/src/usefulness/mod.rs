//! Determining the validity of piecewise closure expressions,
//! i.e. all branches are useful and no case is left unmatched on.
//!
//! Algorithm:
//! https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html
use crate::{ctx, Expr, ExprArm, ExprFields, ExprKind, Pat, PatKind, Ty, Type, TypeKind};
use smallvec::SmallVec;
use std::{convert::TryInto, fmt};

mod error;
pub use error::{RedundentArmError, UsefulnessError, UsefulnessErrors};

#[derive(Copy, Clone, Debug)]
enum Constructor {
    /// Types with a single constructor.
    /// Just tuples for now.
    Single,
    /// A boolean literal.
    Bool(bool),
    /// An integer literal.
    Int(i32),
    /// A choice variant
    ChoiceVariant(u32),
    /// A wildcard.
    /// Can either be an unbound ident ("a") or an actual wildcard ("_").
    Wildcard,
}

use Constructor::*;

#[derive(Copy, Clone, Debug)]
enum Pattern<'cx> {
    Pat(&'cx Pat<'cx>),
    /// Automatically match on everything
    Wildcard(Type<'cx>),
}

#[derive(Debug)]
struct Specialization<'cx> {
    stack_id: usize,
    original_len: usize,
    specialization: Option<Pattern<'cx>>,
}

struct Matrix<'q, 'cx> {
    /// Indices into the pattern stacks that we care about
    specializations: Vec<Specialization<'cx>>,
    q: Specialization<'cx>,
    /// Vector of all pattern stacks
    patstacks: &'q mut [Vec<Pattern<'cx>>],
}

#[derive(Debug)]
pub enum Usefulness {
    // A list of witnesses
    Useful,
    // Which arms make it useless
    Not(SmallVec<[usize; 1]>),
}

impl Usefulness {
    pub fn is_useful(&self) -> bool {
        matches!(self, Usefulness::Useful)
    }
}

/// Push wildcards to the stack for a given type so that it will always match
fn push_wildcard_fields_for_type<'cx>(
    kind: &TypeKind<'cx>,
    stack: &mut Vec<Pattern<'cx>>,
    ctx: &ctx::Typeck<'cx>,
) {
    match kind {
        TypeKind::Var(var) => push_wildcard_fields_for_type(
            &ctx[*var].binding().expect("unbound type var").kind,
            stack,
            ctx,
        ),
        TypeKind::Choice(_choice) => {
            todo!("push_wildcard_fields_for_type for choice types- may need to rewrite this fn")
        }
        TypeKind::Record(types) => stack.extend(types.iter().copied().map(Pattern::Wildcard)),
        _ => {}
    }
}

impl<'cx> Pattern<'cx> {
    /// Used on q to visit q's ctors, where `is_ctor_useful` will
    /// determine if that ctor is useful w.r.t. all other p's.
    fn visit_ctors(
        &self,
        hir: &ctx::Typeck<'cx>,
        mut is_ctor_useful: impl FnMut(Constructor) -> Usefulness,
    ) -> Usefulness {
        match self.ctor() {
            Wildcard => Self::visit_ctors_for_type(&self.ty_kind(), hir, is_ctor_useful),
            other => is_ctor_useful(other),
        }
    }

    /// Only used in `Pattern::visit_ctors`.
    fn visit_ctors_for_type(
        kind: &TypeKind<'cx>,
        ctx: &ctx::Typeck<'cx>,
        mut is_ctor_useful: impl FnMut(Constructor) -> Usefulness,
    ) -> Usefulness {
        match kind {
            TypeKind::I32 => is_ctor_useful(Wildcard),
            TypeKind::Bool => {
                // only need one of them to be useful
                if is_ctor_useful(Bool(true)).is_useful() {
                    Usefulness::Useful
                } else {
                    is_ctor_useful(Bool(false))
                }
            }
            TypeKind::Var(var) => Self::visit_ctors_for_type(
                &ctx[*var].binding().expect("unbound var").kind,
                ctx,
                is_ctor_useful,
            ),
            TypeKind::Record(_) => is_ctor_useful(Single),
            TypeKind::Choice(choice) => {
                let last_index = choice
                    .variants
                    .len()
                    .checked_sub(1)
                    .expect("Must have at least one variant, otherwise typeck would have failed")
                    .try_into()
                    .expect("Variant count doesn't fit in u32");

                for variant in 0..last_index {
                    if is_ctor_useful(ChoiceVariant(variant)).is_useful() {
                        return Usefulness::Useful;
                    }
                }

                is_ctor_useful(ChoiceVariant(last_index))
            }
            TypeKind::Function(_) => is_ctor_useful(Wildcard),
        }
    }

    fn ty_kind(&self) -> TypeKind<'cx> {
        match self {
            Pattern::Pat(pat) => pat.ty().kind,
            Pattern::Wildcard(ty) => ty.kind,
        }
    }

    fn ctor(&self) -> Constructor {
        match self {
            Pattern::Pat(pat) => match pat.kind {
                PatKind::Bool(b) => Bool(b),
                PatKind::I32(i) => Int(i),
                PatKind::Tuple { .. } => Single,
                PatKind::Ident { .. } => Wildcard,
                PatKind::Choice { variant, .. } => ChoiceVariant(variant),
            },
            Pattern::Wildcard(_) => Wildcard,
        }
    }

    /// Pushes the fields of a pattern onto the stack.
    /// This function should only be called when it matches the q.
    ///
    /// For example, pushing the fields of a tuple to the pat stack.
    /// [(a, b)]
    /// [a, b]
    fn push_fields(&self, stack: &mut Vec<Pattern<'cx>>, ctx: &ctx::Typeck<'cx>) {
        match self {
            Pattern::Pat(pat) => pat_kind_push_fields(&pat.kind, stack, ctx),
            Pattern::Wildcard(ty) => push_wildcard_fields_for_type(&ty.kind, stack, ctx),
        }
    }
}

fn pat_kind_push_fields<'cx>(
    kind: &PatKind<'cx>,
    stack: &mut Vec<Pattern<'cx>>,
    ctx: &ctx::Typeck<'cx>,
) {
    match kind {
        PatKind::Tuple { pats, .. } => stack.extend(pats.iter().map(Pattern::Pat)),
        PatKind::Ident { ty, .. } => push_wildcard_fields_for_type(ty, stack, ctx),
        PatKind::Choice {
            payload: Some(pat), ..
        } => pat_kind_push_fields(&pat.kind, stack, ctx),
        _ => {}
    }
}

impl<'cx> Specialization<'cx> {
    fn new(id: usize) -> Self {
        Specialization {
            stack_id: id,
            original_len: 2, // 2 because there's a lhs and rhs
            specialization: None,
        }
    }

    /// Takes a pattern off the stack
    fn specialize(
        &self,
        stacks: &mut [Vec<Pattern<'cx>>],
    ) -> Option<(Specialization<'cx>, Pattern<'cx>)> {
        let stack = &mut stacks[self.stack_id];
        let popped = stack.pop()?;
        let original_len = stack.len();
        Some((
            Specialization {
                stack_id: self.stack_id,
                original_len,
                specialization: Some(popped),
            },
            popped,
        ))
    }

    /// Returns `Some` if values with the given ctor would match on this `PatternStack`.
    fn specialize_if_matching(
        &mut self,
        ctor: Constructor,
        stacks: &mut [Vec<Pattern<'cx>>],
        ctx: &ctx::Typeck<'cx>,
    ) -> Option<Specialization<'cx>> {
        let (patstack, self_pat) = self
            .specialize(stacks)
            .expect("we only call specialize on PatternStacks with an element in them");

        let did_expand: bool = match (ctor, self_pat.ctor()) {
            (Single, Single) => {
                self_pat.push_fields(&mut stacks[patstack.stack_id], ctx);
                true
            }
            (Bool(a), Bool(b)) => a == b,
            (Int(a), Int(b)) => a == b,
            (ChoiceVariant(v1), ChoiceVariant(v2)) => {
                if v1 == v2 {
                    // TODO(quinn): the problem is this fn call right here.
                    // I think that we don't want to be calling `push_fields` here since
                    // we already know its a ChoiceVariant.
                    //
                    // What I want is a way to just push the pattern of the payload,
                    // if there is one. Otherwise push nothing.
                    // But in this match arm, I've already lost any information about
                    // the payload and am just left with the indices (v1/v2) of which
                    // variant is present. I want to maintain information about the payload
                    // pattern, without bundling it into `Constructor::ChoiceVariant`.
                    //
                    // This issue is also impacted by the case below where
                    // `ctor` is a wildcard and `self_pat.ctor()` isn't...
                    self_pat.push_fields(&mut stacks[patstack.stack_id], ctx);
                    true
                } else {
                    false
                }
            }
            (Wildcard, pat) => {
                // If the specialization is a wildcard, then it means that the particular
                // type can only be exhausted by a wildcard. For example, integers
                // (I'm not checking if they cover the required 2^n cases above. Sorry)
                // and functions, which can't be matched on by anything _but_ a wildcard.
                matches!(pat, Wildcard)
            }
            (_pat, Wildcard) => {
                // the pattern above is a wildcard and the q isn't
                // Example:
                // match Some(4) {
                //     _ => {}
                //     Some(4) => {}
                // }
                // In this case, we want to extend the fields with wildcards as necessary
                // so we get the following:
                // match Some(4) {
                //     Some(_) => {}
                //     Some(4) => {}
                // }
                // Then we would do the _ and the 4, showing that the second branch isn't useful.
                // Can't short circuit because there might be other elements in the patstack
                // that aren't wildcarded.
                self_pat.push_fields(&mut stacks[patstack.stack_id], ctx);
                true
            }
            _ => unreachable!("different type patterns, this shouldn't be possible after typeck"),
        };

        if did_expand {
            Some(patstack)
        } else {
            // We popped a thing off the stack, but didn't end up expanding it
            // because it didn't match, so don't forget to put it back :)
            stacks[patstack.stack_id].push(self_pat);
            None
        }
    }

    fn cleanup(self, stacks: &mut [Vec<Pattern<'cx>>]) {
        let stack = &mut stacks[self.stack_id];
        stack.truncate(self.original_len);
        if let Some(pat) = self.specialization {
            stack.push(pat);
        }
    }
}

impl fmt::Debug for Matrix<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = f.debug_struct("Matrix");
        for (i, spec) in self.specializations.iter().enumerate() {
            debug.field(&format!("p_{i}"), &self.patstacks[spec.stack_id]);
        }
        debug.field("q", &self.patstacks[self.q.stack_id]).finish()
    }
}

impl<'cx> Matrix<'_, 'cx> {
    fn sanity_check(&self, msg: &str) {
        let len = self.patstacks[self.q.stack_id].len();
        for spec in self.specializations.iter() {
            debug_assert!(
                len == self.patstacks[spec.stack_id].len(),
                "Not all stacks are the same length: {msg}"
            );
        }
    }

    fn update_q(&mut self, q: Specialization<'cx>) {
        let p_n = std::mem::replace(&mut self.q, q);
        self.specializations.push(p_n);
    }

    /// Returns the usefulness of `self.q` w.r.t. the other patterns.
    fn run(&mut self, hir: &ctx::Typeck<'cx>) -> Usefulness {
        self.sanity_check("start of run");
        let Some((q_prime_generalized, q_pat)) = self.q.specialize(self.patstacks) else {
            // Nothing left to specialize on, are we unique?
            // We're unique if there's nothing above us anymore.
            return if self.specializations.is_empty() { Usefulness::Useful } else {
                Usefulness::Not(self.specializations.iter().map(|spec| spec.stack_id).collect())
            };
        };

        q_prime_generalized.cleanup(self.patstacks);

        q_pat.visit_ctors(hir, |ctor| {
            let q_prime = self
                .q
                .specialize_if_matching(ctor, self.patstacks, hir)
                .expect("ctors came from q, so this should work");

            let specializations = self
                .specializations
                .iter_mut()
                .filter_map(|spec| spec.specialize_if_matching(ctor, self.patstacks, hir))
                .collect();

            // We need to be useful w.r.t _all_ arms above

            let mut m = Matrix {
                specializations,
                q: q_prime,
                patstacks: self.patstacks,
            };

            let is_useful = m.run(hir);

            m.sanity_check("right after running");

            // clear the specializations
            m.cleanup();

            self.sanity_check("pushed things back");

            is_useful
        })
    }

    fn cleanup(self) {
        for spec in self.specializations {
            spec.cleanup(self.patstacks);
        }

        self.q.cleanup(self.patstacks);
    }
}

fn check_usefulness<'cx>(
    arms: &'cx [ExprArm<'cx>],
    ctx: &ctx::Typeck<'cx>,
) -> Result<(), UsefulnessError<'cx>> {
    debug_assert!(arms.len() >= 1, "closures must have at least 1 arm");
    let mut stacks = arms
        .iter()
        .map(|arm| vec![Pattern::Pat(&arm.lhs), Pattern::Pat(&arm.rhs)])
        .collect::<Vec<_>>();

    // Dummy wildcard type for the end to check exhaustiveness
    stacks.push(vec![
        Pattern::Wildcard(arms[0].lhs.ty()),
        Pattern::Wildcard(arms[0].rhs.ty()),
    ]);

    let mut m = Matrix {
        specializations: Vec::with_capacity(stacks.len()),
        q: Specialization::new(0),
        patstacks: stacks.as_mut_slice(),
    };

    // We expect no redundent arms in the hot path
    let mut redundent_arms = Vec::with_capacity(0);

    for (arm, id) in arms.iter().zip(1..) {
        if let Usefulness::Not(indices_of_coverers) = m.run(ctx) {
            redundent_arms.push(RedundentArmError {
                coverers: indices_of_coverers.iter().map(|&idx| &arms[idx]).collect(),
                redundent_arm: arm,
            });
        }

        m.update_q(Specialization::new(id));

        for stack in m.patstacks.iter_mut() {
            debug_assert_eq!(stack.len(), 2);
        }
    }

    let dummy_wildcard_is_useful = m.run(ctx).is_useful();

    if redundent_arms.is_empty() && !dummy_wildcard_is_useful {
        Ok(())
    } else {
        Err(UsefulnessError {
            redundent_arms,
            non_exhaustive: dummy_wildcard_is_useful.then_some(arms.last().unwrap()),
        })
    }
}

fn check_matches_in_expr<'cx>(
    expr: &Expr<'cx>,
    ctx: &ctx::Typeck<'cx>,
    errors: &mut Vec<UsefulnessError<'cx>>,
) {
    match expr.kind {
        ExprKind::Builtin(_) | ExprKind::I32(_) | ExprKind::Bool(_) | ExprKind::Ident { .. } => {}
        ExprKind::Record { exprs, .. } => exprs
            .iter()
            .for_each(|expr| check_matches_in_expr(expr, ctx, errors)),
        ExprKind::Constructor { fields, .. } => match fields {
            ExprFields::Newtype(expr) => check_matches_in_expr(expr, ctx, errors),
            ExprFields::Record(fields) => fields
                .iter()
                .for_each(|field| check_matches_in_expr(field, ctx, errors)),
        },
        ExprKind::Closure { arms, .. } => {
            if let Err(report) = check_usefulness(arms, ctx) {
                errors.push(report);
            }
        }
        ExprKind::Appl { appl, .. } => {
            check_matches_in_expr(&appl.lhs, ctx, errors);
            check_matches_in_expr(&appl.rhs, ctx, errors);
            check_matches_in_expr(&appl.function, ctx, errors);
        }
    }
}

pub fn check<'cx>(
    exprs: impl Iterator<Item = &'cx Expr<'cx>>,
    ctx: &ctx::Typeck<'cx>,
) -> Vec<UsefulnessError<'cx>> {
    let mut errors = Vec::with_capacity(0);
    for expr in exprs {
        check_matches_in_expr(expr, ctx, &mut errors);
    }
    errors
}
