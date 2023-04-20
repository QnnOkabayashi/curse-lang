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

use Constructor::*;

#[derive(Copy, Clone, Debug)]
enum Pattern<'hir> {
    Pat(&'hir Pat<'hir, 'hir>),
    /// Automatically match on everything
    Wildcard(Type<'hir>),
}

fn expand_ctors_for_type(kind: &TypeKind<'_>) -> SmallVec<[Constructor; 2]> {
    match kind {
        TypeKind::I32 => smallvec![Wildcard],
        TypeKind::Bool => smallvec![Bool(true), Bool(false)],
        TypeKind::Unit => smallvec![Single],
        TypeKind::Var(_) => todo!(),
        TypeKind::Tuple(_) => smallvec![Single],
        TypeKind::Function(_) => smallvec![Wildcard],
    }
}

/// Push wildcards to the stack for a given type so that it will always match
fn push_wildcard_fields_for_type<'hir>(kind: &TypeKind<'hir>, stack: &mut Vec<Pattern<'hir>>) {
    match kind {
        TypeKind::I32 | TypeKind::Bool | TypeKind::Unit | TypeKind::Function(_) => {}
        TypeKind::Var(_) => todo!(),
        TypeKind::Tuple(tuple) => stack.extend(tuple.iter().copied().map(Pattern::Wildcard)),
    }
}

impl<'hir> Pattern<'hir> {
    fn expand_ctors(&self) -> SmallVec<[Constructor; 2]> {
        match self.ctor() {
            Wildcard => expand_ctors_for_type(&self.ty_kind()),
            other => smallvec![other],
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
    fn push_fields(&self, stack: &mut Vec<Pattern<'hir>>) {
        match self {
            Pattern::Pat(pat) => match pat.kind {
                PatKind::Bool(_) | PatKind::I32(_) | PatKind::Unit => {}
                PatKind::Tuple { pats, .. } => stack.extend(pats.iter().map(Pattern::Pat)),
                PatKind::Ident { ty, .. } => push_wildcard_fields_for_type(&ty.kind, stack),
            },
            Pattern::Wildcard(ty) => push_wildcard_fields_for_type(&ty.kind, stack),
        }
    }
}

#[derive(Debug)]
struct Specialization<'hir> {
    stack: usize,
    original_len: usize,
    specialization: Option<Pattern<'hir>>,
}

impl<'hir> Specialization<'hir> {
    fn new_from_id(id: usize) -> Self {
        Specialization {
            stack: id,
            original_len: 1,
            specialization: None,
        }
    }

    fn specialize(
        &self,
        stacks: &mut [Vec<Pattern<'hir>>],
    ) -> Option<(Specialization<'hir>, Pattern<'hir>)> {
        let stack = &mut stacks[self.stack];
        let popped = stack.pop()?;
        let original_len = stack.len();
        Some((
            Specialization {
                stack: self.stack,
                original_len,
                specialization: Some(popped),
            },
            popped,
        ))
    }

    /// Remove any elements added in this scope by truncating to the original length.
    fn clear_scope(&mut self, stacks: &mut [Vec<Pattern<'hir>>]) {
        stacks[self.stack].truncate(self.original_len);
    }

    /// Returns `Some` if values with the given ctor would match on this `PatternStack`.
    fn specialize_if_matching(
        &mut self,
        specialization: Constructor,
        stacks: &mut [Vec<Pattern<'hir>>],
    ) -> Option<Specialization<'hir>> {
        let (patstack, pat) = self
            .specialize(stacks)
            .expect("we only call specialize on PatternStacks with an element in them");
        // when we scope, we should also return the think that we specialized on
        match (specialization, pat.ctor()) {
            (Single, Single) => pat.push_fields(&mut stacks[patstack.stack]),
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
                pat.push_fields(&mut stacks[patstack.stack]);
            }
            _ => panic!("different type patterns, this shouldn't be possible after typeck"),
        }
        Some(patstack)
    }
}

#[derive(Debug)]
struct Matrix<'q, 'hir> {
    /// Indices into the pattern stacks that we care about
    specializations: Vec<Specialization<'hir>>,
    q: Specialization<'hir>,
    /// Vector of all pattern stacks
    stacks: &'q mut Vec<Vec<Pattern<'hir>>>,
}

impl Drop for Matrix<'_, '_> {
    fn drop(&mut self) {
        for spec in self.specializations.iter() {
            let stack = &mut self.stacks[spec.stack];
            stack.truncate(spec.original_len);
            if let Some(pat) = spec.specialization {
                stack.push(pat);
            }
        }
    }
}

#[derive(Debug)]
enum Usefulness {
    Useful,
    Not,
}

impl<'hir> Matrix<'_, 'hir> {
    fn sanity_check(&self) {
        let len = self.stacks[self.q.stack].len();
        for rem in self.specializations.iter() {
            assert_eq!(len, self.stacks[rem.stack].len());
        }
    }

    fn run(&mut self) -> Usefulness {
        self.sanity_check();
        let Some((q_prime_generalized, pat)) = self.q.specialize(self.stacks) else {
            // Nothing left to specialize on, are we unique?
            // We're unique if there's nothing above us anymore.
            return if self.specializations.is_empty() { Usefulness::Useful } else {
                println!("not useful because there are remaining arms above");
                Usefulness::Not
            };
        };

        let ctors = pat.expand_ctors();

        let stack = &mut self.stacks[q_prime_generalized.stack];
        stack.truncate(q_prime_generalized.original_len);
        stack.push(pat);
        drop(q_prime_generalized);

        for ctor in ctors {
            let q_prime = self
                .q
                .specialize_if_matching(ctor, self.stacks)
                .expect("ctors came from q, so this should work");
            let specializations = self
                .specializations
                .iter_mut()
                .filter_map(|spec| spec.specialize_if_matching(ctor, self.stacks))
                .collect();

            let mut m = Matrix {
                specializations,
                q: q_prime,
                stacks: self.stacks,
            };

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

    let ty_bool_bool = Type {
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
    };

    let p_1 = Pat {
        kind: PatKind::Tuple {
            ty: ty_bool_bool,
            pats: &List {
                item: Pat {
                    kind: PatKind::Bool(true),
                    span: (0, 0),
                },
                next: Some(&List {
                    item: Pat {
                        kind: PatKind::Bool(true),
                        span: (0, 0),
                    },
                    next: None,
                }),
            },
        },
        span: (0, 0),
    };

    let p_2 = Pat {
        kind: PatKind::Tuple {
            ty: ty_bool_bool,
            pats: &List {
                item: Pat {
                    kind: PatKind::Bool(true),
                    span: (0, 0),
                },
                next: Some(&List {
                    item: Pat {
                        kind: PatKind::Bool(false),
                        span: (0, 0),
                    },
                    next: None,
                }),
            },
        },
        span: (0, 0),
    };

    let p_3 = Pat {
        kind: PatKind::Tuple {
            ty: ty_bool_bool,
            pats: &List {
                item: Pat {
                    kind: PatKind::Bool(false),
                    span: (0, 0),
                },
                next: Some(&List {
                    item: Pat {
                        kind: PatKind::Bool(true),
                        span: (0, 0),
                    },
                    next: None,
                }),
            },
        },
        span: (0, 0),
    };

    let q = Pat {
        kind: PatKind::Tuple {
            ty: ty_bool_bool,
            pats: &List {
                item: Pat {
                    kind: PatKind::Bool(false),
                    span: (0, 0),
                },
                next: Some(&List {
                    item: Pat {
                        kind: PatKind::Bool(false),
                        span: (0, 0),
                    },
                    next: None,
                }),
            },
        },
        span: (0, 0),
    };

    let mut stacks = vec![
        vec![Pattern::Pat(&p_1)],
        vec![Pattern::Pat(&p_2)],
        vec![Pattern::Pat(&p_3)],
        vec![Pattern::Pat(&q)],
    ];

    let mut m = Matrix {
        specializations: (0..stacks.len() - 1)
            .map(|stack| Specialization {
                stack,
                original_len: 1,
                specialization: None,
            })
            .collect(),
        q: Specialization {
            stack: stacks.len() - 1,
            original_len: 1,
            specialization: None,
        },
        stacks: &mut stacks,
    };

    let usefulness = m.run();

    println!("{usefulness:?}");
}

// Algorithm:
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html
// only specialize on the constructor that q uses.
