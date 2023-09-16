use curse_hir::hir;
use curse_interner::InternedString;
use std::cmp::Ordering;

use crate::gensym;

#[cfg(test)]
mod tests;

type Variable = InternedString;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binding {
    pub variable: Variable,
    pub value: BindingValue,
}

impl Binding {
    fn new(variable: Variable, value: BindingValue) -> Self {
        Self { variable, value }
    }
}

// Choice types will be represented as records whose first entry is an integer tag, so for the
// purposes of binding we can just use `Record`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BindingValue {
    Variable(Variable),
    Record { name: Variable, index: usize },
}

/// The expression to evaluate once reaching the end of the tree with the bindings accrued
/// along the way.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Body<'hir> {
    pub value: hir::Expr<'hir>,
    pub bindings: Vec<Binding>,
}

impl<'hir> Body<'hir> {
    fn new(value: hir::Expr<'hir>, bindings: Vec<Binding>) -> Self {
        Self { value, bindings }
    }
}

/// The constructors we can compare values against. Although choice types will be represented with
/// records, we're still working with the hir at this point which distinguishes them, which comes
/// in handy for making better decision trees.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Constructor<'hir> {
    Integer(u32),
    Boolean(bool),
    Record(Vec<Constructor<'hir>>),
    NamedConstructor(hir::Path<'hir>, Box<Constructor<'hir>>),
    Variable(Variable),
}

impl<'hir> Constructor<'hir> {
    fn from_pattern(pat: hir::PatKind<'hir>) -> Constructor<'hir> {
        match pat {
            hir::PatKind::Lit(hir::Lit::Integer(n)) => Constructor::Integer(n),
            hir::PatKind::Lit(hir::Lit::Bool(b)) => Constructor::Boolean(b),
            hir::PatKind::Lit(hir::Lit::Ident(id)) => Constructor::Variable(id.symbol),
            hir::PatKind::Record(map) => Constructor::Record(
                map.entries
                    .into_iter()
                    .map(|x| match *x {
                        (_, Some(pat)) => Constructor::from_pattern(pat.kind.clone()),
                        (id, None) => Constructor::Variable(id.symbol),
                    })
                    .collect(),
            ),
            hir::PatKind::Constructor(path, pat) => Constructor::NamedConstructor(
                path,
                Box::new(Constructor::from_pattern(pat.kind.clone())),
            ),
            hir::PatKind::Error => todo!(),
        }
    }

    fn matches(&self, other: &Constructor<'hir>) -> bool {
        match (self, other) {
            (Constructor::Integer(n), Constructor::Integer(m)) => n == m,
            (Constructor::Boolean(b1), Constructor::Boolean(b2)) => b1 == b2,
            // type checking I think guarantees that the records must match
            (Constructor::Record(_), Constructor::Record(_)) => true,
            // this works because of our definition of `Eq` on `Ident`
            (Constructor::NamedConstructor(path1, _), Constructor::NamedConstructor(path2, _)) => {
                path1 == path2
            }
            (Constructor::Variable(_), _) => true,
            _ => false,
        }
    }
}

/// The actual decision tree. At each step, we will compare a single variable against a single
/// constructor. If it matches, we head down one subtree, and if it fails we head down another. If
/// the list of variables and constructors is ever empty, then we are done and have successfully
/// matched. On the other hand, if the remaining list of arms is empty, then our original set of
/// arms must not have been exhaustive.
#[derive(Debug, PartialEq, Eq)]
pub enum Decision<'hir> {
    // note: if the leaves of the tree don't contain one of the original bodies, then the case
    // that body belonged to was redundant, giving us a neat way to check for redundancy
    Success(Body<'hir>),
    Failure,
    Branch {
        test: Test<'hir>,
        match_path: Box<Decision<'hir>>,
        fail_path: Box<Decision<'hir>>,
    },
}

/// One single comparison.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Test<'hir> {
    pub variable: Variable,
    pub constructor: Constructor<'hir>,
}

impl<'hir> Test<'hir> {
    fn new(variable: Variable, constructor: Constructor<'hir>) -> Self {
        Self {
            variable,
            constructor,
        }
    }
}

/// A clause can have multiple tests and then a body to execute if it matches.
#[derive(Clone)]
struct Clause<'hir> {
    tests: Vec<Test<'hir>>,
    body: Body<'hir>,
}

impl<'hir> Clause<'hir> {
    fn new(tests: Vec<Test<'hir>>, body: Body<'hir>) -> Self {
        Self { tests, body }
    }

    fn from_arm(arm: &hir::Arm<'hir>, left_variable: Variable, right_variable: Variable) -> Self {
        let (left_cons, right_cons) = match arm.params {
            &[] => (Constructor::Integer(0), Constructor::Integer(0)),
            &[left] => (
                Constructor::from_pattern(left.pat.kind.clone()),
                Constructor::Integer(0),
            ),
            &[left, right] => (
                Constructor::from_pattern(left.pat.kind.clone()),
                Constructor::from_pattern(right.pat.kind.clone()),
            ),
            _ => unreachable!("will only ever be 0, 1, or 2"),
        };

        Self {
            tests: vec![
                Test::new(left_variable, left_cons),
                Test::new(right_variable, right_cons),
            ],
            body: Body::new(*arm.body, vec![]),
        }
    }

    fn bind_bare_variables(&mut self) {
        // for each test, if the test is against a variable, get rid of it and insert a
        // new binding into the body of the clause
        self.tests.retain(|test| {
            if let Constructor::Variable(var) = test.constructor {
                self.body
                    .bindings
                    .push(Binding::new(var, BindingValue::Variable(test.variable)));
                false
            } else {
                true
            }
        });
    }
}

type MatchExpr<'hir> = Vec<Clause<'hir>>;

pub fn compile_match_expr<'hir>(
    hir_closure: &'hir [hir::Arm<'hir>],
    left: Variable,
    right: Variable,
) -> Decision<'hir> {
    let match_expr = hir_closure
        .into_iter()
        .map(|arm| Clause::from_arm(arm, left, right))
        .collect();

    compile_match(match_expr)
}

fn compile_match<'hir>(mut match_expr: MatchExpr<'hir>) -> Decision<'hir> {
    // base case
    if match_expr.is_empty() {
        return Decision::Failure;
    }

    // step 1
    for clause in match_expr.iter_mut() {
        clause.bind_bare_variables();
    }

    // other base case
    if match_expr[0].tests.is_empty() {
        return Decision::Success(match_expr[0].body.clone());
    }

    // step 2
    let test_idx = select_test(&match_expr);
    let test = match_expr[0].tests[test_idx].clone();

    let mut a: MatchExpr<'hir> = vec![];
    let mut b: MatchExpr<'hir> = vec![];

    // step 4
    'outer: for clause in match_expr.iter_mut() {
        for (idx, new_test) in clause.tests.iter().enumerate() {
            if new_test.variable == test.variable {
                // case (a) of step 4
                if new_test.constructor.matches(&test.constructor) {
                    let mut new_tests = vec![];
                    match &new_test.constructor {
                        Constructor::Integer(_) | Constructor::Boolean(_) => (),
                        Constructor::Record(ctors) => {
                            new_tests = ctors
                                .iter()
                                .enumerate()
                                .map(|(index, ctor)| {
                                    let r = gensym("r");
                                    clause.body.bindings.push(Binding::new(
                                        r,
                                        BindingValue::Record {
                                            name: test.variable,
                                            index,
                                        },
                                    ));
                                    Test::new(r, ctor.clone())
                                })
                                .collect()
                        }
                        Constructor::NamedConstructor(_, ctor) => {
                            let c = gensym("c");
                            clause
                                .body
                                .bindings
                                .push(Binding::new(c, BindingValue::Variable(test.variable)));
                            new_tests.push(Test::new(c, (**ctor).clone()))
                        }
                        Constructor::Variable(_) => unreachable!("already pushed vars to body"),
                    }
                    clause.tests.remove(idx);
                    new_tests.append(&mut clause.tests);
                    a.push(Clause::new(new_tests, clause.body.clone()));
                    continue 'outer;
                } else {
                    // case (b) of step 4
                    b.push(clause.clone());
                    continue 'outer;
                }
            }
        }

        // only here if none of the tests in `clause` were against `test.variable`,
        // leading to case (c) of step 4
        // the point of the heuristic `select_test` is to minimize this
        a.push(clause.clone());
        b.push(clause.clone());
    }

    // step 3 (and 5)
    Decision::Branch {
        test: test.clone(),
        match_path: Box::new(compile_match(a)),
        fail_path: Box::new(compile_match(b)),
    }
}

// given a `MatchExpr`, finds the index of the best test to do first from the first clause
fn select_test<'hir>(match_expr: &MatchExpr<'hir>) -> usize {
    let mut counts = vec![0usize; match_expr[0].tests.len()];

    for clause in &match_expr[1..] {
        for test in &clause.tests {
            for (idx, test2) in match_expr[0].tests.iter().enumerate() {
                if test2.variable == test.variable {
                    counts[idx] += 1;
                }
            }
        }
    }

    counts
        .iter()
        .enumerate()
        .max_by(|(_, &a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal))
        .map(|(index, _)| index)
        .unwrap_or(0)
}
