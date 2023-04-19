use std::rc::Rc;

use crate::{Pat, PatKind, Ty, Type, TypeKind};
use smallvec::{smallvec, SmallVec};

// Matching on ints seems kinda hard, so let's just not support it right now.
// This will make matching so much easier because all the constructors are known,
// especially since we don't have custom types yet. There's literally just
// booleans, tuples, and wildcards :)
#[derive(Copy, Clone, Debug)]
enum Constructor {
    /// Types with a single constructor.
    /// Just tuples for now.
    Single,
    Bool(bool),
    /// An integer literal.
    Int(i32),
    /// A wildcard.
    /// Can either be an unbound ident ("a") or an actual wildcard ("_").
    Wildcard,
}

// Lifetimes are destroying me rn and I just want to see this work.
// TODO(quinn): don't use Rc.
#[derive(Clone, Debug)]
struct DeconstructedPat<'hir> {
    ctor: Constructor,
    fields: Rc<Vec<DeconstructedPat<'hir>>>,
    ty: Type<'hir>,
}

impl<'hir> DeconstructedPat<'hir> {
    fn from_pat(pat: &Pat<'hir, '_>) -> DeconstructedPat<'hir> {
        match pat.kind {
            PatKind::Bool(b) => DeconstructedPat {
                ctor: Constructor::Bool(b),
                fields: Rc::new(Vec::new()),
                ty: pat.ty(),
            },
            PatKind::I32(int) => DeconstructedPat {
                ctor: Constructor::Int(int),
                fields: Rc::new(Vec::new()),
                ty: pat.ty(),
            },
            PatKind::Unit => DeconstructedPat {
                ctor: Constructor::Single,
                fields: Rc::new(Vec::new()),
                ty: pat.ty(),
            },
            PatKind::Ident { .. } => DeconstructedPat {
                ctor: Constructor::Wildcard,
                fields: Rc::new(Vec::new()),
                ty: pat.ty(),
            },
            PatKind::Tuple { pats, .. } => DeconstructedPat {
                ctor: Constructor::Single,
                fields: Rc::new(pats.iter().map(Self::from_pat).collect()),
                ty: pat.ty(),
            },
        }
    }
}

#[derive(Debug)]
struct PatternStack<'scope, 'hir> {
    stack: &'scope mut Vec<DeconstructedPat<'hir>>,
    original_len: usize,
    /// The item we have to put back on at the end
    specialization: Option<DeconstructedPat<'hir>>,
}

impl Drop for PatternStack<'_, '_> {
    fn drop(&mut self) {
        self.clear_scope();
        if let Some(specialization) = self.specialization.take() {
            self.stack.push(specialization);
        }
    }
}

impl<'hir> PatternStack<'_, 'hir> {
    fn scope(&mut self) -> Option<PatternStack<'_, 'hir>> {
        let popped = self.stack.pop()?;
        let original_len = self.stack.len();
        Some(PatternStack {
            stack: self.stack,
            original_len,
            specialization: Some(popped),
        })
    }

    /// Remove any elements added in this scope by truncating to the original length.
    fn clear_scope(&mut self) {
        self.stack.truncate(self.original_len);
    }

    fn extend(&mut self, pats: impl Iterator<Item = DeconstructedPat<'hir>>) {
        self.stack.extend(pats)
    }

    /// Returns `Some` if values with the given ctor would match on this `PatternStack`.
    fn specialize(&mut self, specialization: Constructor) -> Option<PatternStack<'_, 'hir>> {
        use Constructor::*;
        let mut patstack = self.scope().expect("should still be one there");
        let pat = patstack.specialization.as_ref().unwrap().clone();
        match (specialization, pat.ctor) {
            (Single, Single) => patstack.extend(pat.fields.iter().cloned()),
            (Bool(true), Bool(true)) | (Bool(false), Bool(false)) => {}
            (Bool(true), Bool(false)) | (Bool(false), Bool(true)) => return None, // not a match
            (Int(a), Int(b)) => {
                if a != b {
                    // not a match
                    return None;
                }
                // is a match but we don't add any extra patterns
            }
            (Wildcard, pat) => {
                // If the specialization is a wildcard, then it means that the particular
                // type can only be exhausted by a wildcard. For example, integers
                // (I'm not checking if they cover the required 2^n cases above. Sorry)
                // and functions, which can't be matched on by anything _but_ a wildcard.

                if let Wildcard = pat {
                    // If we're a wildcard, then we cover any values that the specialization
                    // might cover, so we're useful.
                } else {
                    // Otherwise, we aren't useful.
                    return None;
                }
            }
            _ => panic!("different type patterns, this shouldn't be possible after typeck"),
        }
        Some(patstack)
    }
}

#[derive(Debug)]
struct Matrix<'q, 'ctor_loop, 'scope, 'hir> {
    // invariant: all pattern stacks should be the same length
    remaining: Vec<PatternStack<'ctor_loop, 'hir>>,
    q: &'q mut PatternStack<'scope, 'hir>,
}

#[derive(Debug)]
enum Usefulness {
    Useful,
    Not,
}

impl Matrix<'_, '_, '_, '_> {
    fn sanity_check(&self) {
        let len = self.q.stack.len();
        for rem in self.remaining.iter() {
            assert_eq!(rem.stack.len(), len);
        }
    }

    fn run(&mut self) -> Usefulness {
        use Constructor::*;
        // for each pattern in q's
        let Some(q_prime_generalized) = self.q.scope() else {
            // Nothing left to specialize on, are we unique?
            // We're unique if there's nothing above us anymore.
            return if self.remaining.is_empty() { Usefulness::Useful } else {
                println!("not useful because there are remaining arms above");
                Usefulness::Not
            };
        };

        // While we could use static slices right now, we'll eventually want to support
        // enums which will require a variable number of constructors to try.
        // So we'll use smallvec instead.
        let pat = q_prime_generalized.specialization.as_ref().unwrap();
        let ctors: SmallVec<[Constructor; 2]> = match pat.ctor {
            Wildcard => match pat.ty.kind {
                TypeKind::I32 => smallvec![Wildcard],
                TypeKind::Bool => smallvec![Bool(true), Bool(false)],
                TypeKind::Unit => smallvec![Single],
                TypeKind::Var(_) => todo!("type resolution"),
                TypeKind::Tuple(_) => smallvec![Single],
                TypeKind::Function(_) => smallvec![Wildcard],
            },
            other => smallvec![other],
        };

        drop(q_prime_generalized);

        for ctor in ctors {
            let mut q_prime = self
                .q
                .specialize(ctor)
                .expect("the ctor was created by q, so it should always match");
            let specializations = self
                .remaining
                .iter_mut()
                .filter_map(|patstack| patstack.specialize(ctor))
                .collect();

            let mut m = Matrix {
                remaining: specializations,
                q: &mut q_prime,
            };

            m.sanity_check();

            if let Usefulness::Not = m.run() {
                println!("not useful recursively when specialized with {ctor:?}");
                return Usefulness::Not;
            }

            // The lifetime 'ctor_loop ends here
        }

        Usefulness::Useful
    }
}

#[test]
fn test() {
    use crate::List;

    macro_rules! bool_bool {
        ($a:expr, $b:expr) => {
            PatternStack {
                stack: &mut vec![DeconstructedPat::from_pat(&Pat {
                    kind: PatKind::Tuple {
                        ty: Type {
                            kind: TypeKind::Tuple(&List {
                                item: Type {
                                    kind: TypeKind::Bool,
                                    span: (0, 0),
                                },
                                next: Some(&List {
                                    item: Type {
                                        kind: TypeKind::Bool,
                                        span: (0, 0),
                                    },
                                    next: None,
                                }),
                            }),
                            span: (0, 0),
                        },
                        pats: &List {
                            item: Pat {
                                kind: PatKind::Bool($a),
                                span: (0, 0),
                            },
                            next: Some(&List {
                                item: Pat {
                                    kind: PatKind::Bool($b),
                                    span: (0, 0),
                                },
                                next: None,
                            }),
                        },
                    },
                    span: (0, 0),
                })],
                original_len: 1,
                specialization: None,
            }
        }
    }

    let p_1 = bool_bool!(true, true);
    let p_2 = bool_bool!(true, false);
    let p_3 = bool_bool!(false, true);

    let mut q = bool_bool!(true, false);

    let mut m = Matrix {
        remaining: vec![p_1, p_2, p_3],
        q: &mut q,
    };

    let usefulness = m.run();

    println!("{usefulness:?}");
}

// Algorithm:
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html
// only specialize on the constructor that q uses.
