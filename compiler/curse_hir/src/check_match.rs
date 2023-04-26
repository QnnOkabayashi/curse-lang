//! Algorithm:
//! https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html
use crate::{Expr, ExprArm, ExprKind, Hir, List, Pat, PatKind, Ty, Type, TypeKind};
use std::fmt;

#[derive(Copy, Clone, Debug)]
enum Constructor {
    /// Types with a single constructor.
    /// Just tuples for now.
    Single,
    /// A boolean literal.
    Bool(bool),
    /// An integer literal.
    Int(i32),
    /// A wildcard.
    /// Can either be an unbound ident ("a") or an actual wildcard ("_").
    Wildcard,
}

use Constructor::*;

#[derive(Copy, Clone, Debug)]
enum Pattern<'hir> {
    Pat(&'hir Pat<'hir, 'hir>),
    /// Automatically match on everything
    Wildcard(Type<'hir>),
}

#[derive(Debug)]
struct Specialization<'hir> {
    stack_id: usize,
    original_len: usize,
    specialization: Option<Pattern<'hir>>,
}

struct Matrix<'q, 'hir> {
    /// Indices into the pattern stacks that we care about
    specializations: Vec<Specialization<'hir>>,
    q: Specialization<'hir>,
    /// Vector of all pattern stacks
    stacks: &'q mut [Vec<Pattern<'hir>>],
}

#[derive(Debug)]
pub enum Usefulness {
    Useful,
    Not,
}

impl Usefulness {
    pub fn is_useful(&self) -> bool {
        matches!(self, Usefulness::Useful)
    }
}

/// Push wildcards to the stack for a given type so that it will always match
fn push_wildcard_fields_for_type<'hir>(
    kind: &TypeKind<'hir>,
    stack: &mut Vec<Pattern<'hir>>,
    hir: &Hir<'hir, '_>,
) {
    match kind {
        TypeKind::I32 | TypeKind::Bool | TypeKind::Unit | TypeKind::Function(_) => {}
        TypeKind::Var(var) => {
            push_wildcard_fields_for_type(&hir[*var].expect("unbound type var").0.kind, stack, hir)
        }
        TypeKind::Tuple(types) => stack.extend(types.iter().copied().map(Pattern::Wildcard)),
    }
}

impl<'hir> Pattern<'hir> {
    fn visit_ctors(
        &self,
        hir: &Hir<'hir, '_>,
        mut is_ctor_useful: impl FnMut(Constructor) -> Usefulness,
    ) -> Usefulness {
        match self.ctor() {
            Wildcard => Self::visit_ctors_for_type(&self.ty_kind(), hir, is_ctor_useful),
            other => is_ctor_useful(other),
        }
    }

    fn visit_ctors_for_type(
        kind: &TypeKind<'hir>,
        hir: &Hir<'hir, '_>,
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
            TypeKind::Unit => is_ctor_useful(Single),
            TypeKind::Var(var) => Self::visit_ctors_for_type(
                &hir[*var].expect("unbound var").0.kind,
                hir,
                is_ctor_useful,
            ),
            TypeKind::Tuple(_) => is_ctor_useful(Single),
            TypeKind::Function(_) => is_ctor_useful(Wildcard),
        }
    }

    fn ty_kind(&self) -> TypeKind<'hir> {
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
                PatKind::Unit => Single,
                PatKind::Tuple { .. } => Single,
                PatKind::Ident { .. } => Wildcard,
            },
            Pattern::Wildcard(_) => Wildcard,
        }
    }

    /// Pushes the fields of a pattern onto the stack.
    /// This function should only be called when it matches the q.
    fn push_fields(&self, stack: &mut Vec<Pattern<'hir>>, hir: &Hir<'hir, '_>) {
        match self {
            Pattern::Pat(pat) => match pat.kind {
                PatKind::Bool(_) | PatKind::I32(_) | PatKind::Unit => {}
                PatKind::Tuple { pats, .. } => stack.extend(pats.iter().map(Pattern::Pat)),
                PatKind::Ident { ty, .. } => push_wildcard_fields_for_type(&ty.kind, stack, hir),
            },
            Pattern::Wildcard(ty) => push_wildcard_fields_for_type(&ty.kind, stack, hir),
        }
    }
}

impl<'hir> Specialization<'hir> {
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
        stacks: &mut [Vec<Pattern<'hir>>],
    ) -> Option<(Specialization<'hir>, Pattern<'hir>)> {
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
        specialization: Constructor,
        stacks: &mut [Vec<Pattern<'hir>>],
        hir: &Hir<'hir, '_>,
    ) -> Option<Specialization<'hir>> {
        let (patstack, pat) = self
            .specialize(stacks)
            .expect("we only call specialize on PatternStacks with an element in them");
        // when we scope, we should also return the think that we specialized on
        let did_expand: bool = match (specialization, pat.ctor()) {
            (Single, Single) => {
                pat.push_fields(&mut stacks[patstack.stack_id], hir);
                true
            }
            (Bool(a), Bool(b)) => a == b,
            (Int(a), Int(b)) => a == b,
            (Wildcard, pat) => {
                // If the specialization is a wildcard, then it means that the particular
                // type can only be exhausted by a wildcard. For example, integers
                // (I'm not checking if they cover the required 2^n cases above. Sorry)
                // and functions, which can't be matched on by anything _but_ a wildcard.
                matches!(pat, Wildcard)
            }
            (_, Wildcard) => {
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
                pat.push_fields(&mut stacks[patstack.stack_id], hir);
                true
            }
            _ => panic!("different type patterns, this shouldn't be possible after typeck"),
        };
        if did_expand {
            Some(patstack)
        } else {
            // We popped a thing off the stack, but didn't end up expanding it
            // because it didn't match, so don't forget to put it back :)
            stacks[patstack.stack_id].push(pat);
            None
        }
    }

    fn cleanup(self, stacks: &mut [Vec<Pattern<'hir>>]) {
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
            debug.field(&format!("p_{i}"), &self.stacks[spec.stack_id]);
        }
        debug.field("q", &self.stacks[self.q.stack_id]).finish()
    }
}

impl<'hir> Matrix<'_, 'hir> {
    fn sanity_check(&self, msg: &str) {
        let len = self.stacks[self.q.stack_id].len();
        for spec in self.specializations.iter() {
            debug_assert!(
                len == self.stacks[spec.stack_id].len(),
                "Not all stacks are the same length: {msg}"
            );
        }
    }

    fn update_q(&mut self, q: Specialization<'hir>) {
        let p_n = std::mem::replace(&mut self.q, q);
        self.specializations.push(p_n);
    }

    fn run(&mut self, hir: &Hir<'hir, '_>) -> Usefulness {
        self.sanity_check("start of run");
        let Some((q_prime_generalized, pat)) = self.q.specialize(self.stacks) else {
            // Nothing left to specialize on, are we unique?
            // We're unique if there's nothing above us anymore.
            return if self.specializations.is_empty() { Usefulness::Useful } else {
                // println!("not useful because there are remaining arms above");
                Usefulness::Not
            };
        };

        // let stack = &mut self.stacks[q_prime_generalized.stack];
        // stack.truncate(q_prime_generalized.original_len);
        // stack.push(pat);
        // drop(q_prime_generalized);
        q_prime_generalized.cleanup(self.stacks);

        // This could be a regular iterator if we made it so all types
        // are resolved before this stage.
        pat.visit_ctors(hir, |ctor| {
            let q_prime = self
                .q
                .specialize_if_matching(ctor, self.stacks, hir)
                .expect("ctors came from q, so this should work");
            let specializations = self
                .specializations
                .iter_mut()
                .filter_map(|spec| spec.specialize_if_matching(ctor, self.stacks, hir))
                .collect();

            let mut m = Matrix {
                specializations,
                q: q_prime,
                stacks: self.stacks,
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
            spec.cleanup(self.stacks);
        }

        self.q.cleanup(self.stacks);
    }
}

pub fn check_usefulness<'hir>(
    arms: &'hir List<'hir, ExprArm<'hir, '_>>,
    hir: &Hir<'hir, '_>,
) -> Usefulness {
    let mut stacks = arms
        .iter()
        .map(|arm| vec![Pattern::Pat(&arm.lhs), Pattern::Pat(&arm.rhs)])
        .collect::<Vec<_>>();

    let len = stacks.len();
    // We know its not empty because arms is a linked list node containing an element and
    // potentially more.

    // Dummy wildcard type for the end to check exhaustiveness
    let lhs_type = arms.item.lhs.ty();
    let rhs_type = arms.item.rhs.ty();
    stacks.push(vec![
        Pattern::Wildcard(lhs_type),
        Pattern::Wildcard(rhs_type),
    ]);

    let mut m = Matrix {
        specializations: Vec::with_capacity(stacks.len()),
        q: Specialization::new(0),
        stacks: stacks.as_mut_slice(),
    };

    for id in 1..len {
        if let Usefulness::Not = m.run(hir) {
            return Usefulness::Not;
        }
        m.update_q(Specialization::new(id));

        for stack in m.stacks.iter_mut() {
            debug_assert_eq!(stack.len(), 2);
        }
    }

    // run on the last one
    if let Usefulness::Not = m.run(hir) {
        return Usefulness::Not;
    }

    // we also want to try a fake wildcard to check that the match is exhaustive
    m.update_q(Specialization::new(len));

    if m.run(hir).is_useful() {
        println!("the match isn't exhaustive");
        Usefulness::Not
    } else {
        // the dummy `_` pattern was not useful, meaning the match is exhaustive
        Usefulness::Useful
    }
}

pub fn check_matches_in_expr<'hir>(expr: &Expr<'hir, '_>, hir: &Hir<'hir, '_>) -> Usefulness {
    match expr.kind {
        ExprKind::Builtin(_)
        | ExprKind::I32(_)
        | ExprKind::Bool(_)
        | ExprKind::Unit
        | ExprKind::Ident { .. } => Usefulness::Useful,
        ExprKind::Tuple { exprs, .. } => {
            for e in exprs.iter() {
                if let Usefulness::Not = check_matches_in_expr(e, hir) {
                    return Usefulness::Not;
                }
            }
            Usefulness::Useful
        }
        ExprKind::Closure { arms, .. } => check_usefulness(arms, hir),
        ExprKind::Appl { appl, .. } => {
            let lhs = check_matches_in_expr(&appl.lhs, hir);
            let rhs = check_matches_in_expr(&appl.rhs, hir);
            let function = check_matches_in_expr(&appl.function, hir);
            if let (Usefulness::Useful, Usefulness::Useful, Usefulness::Useful) =
                (lhs, rhs, function)
            {
                Usefulness::Useful
            } else {
                Usefulness::Not
            }
        }
    }
}
