use curse_hir::hir;
use curse_interner::InternedString;

/// New binding to introduce in a body. E.g.
/// ```
/// |{a, b}, c| a + b + c
/// ```
/// will have been translated to take two args, probably something like `_x__1` and `_y__2` and
/// the record will have been desugared into basically a tuple. Then `a` will be bound to the
/// equivalent of `_x__1.0`, `b` to `_x__1.1`, and `c` to `_y__2`.
///
/// Note: Choice types at this point are just a record whose first entry is a tag, so we don't
/// actually need to handle that case separately from records.
struct Binding {
    variable: InternedString,
    value: BindingValue,
}

enum BindingValue {
    Variable(InternedString),
    Record { name: InternedString, index: usize },
}

/// The expression to evaluate once reaching the end of the tree. Bindings will have been
/// accruing throughout the course of the decision tree, ultimately leading to all the variables in
/// the body being (hopefully) bound.
struct Body<'hir> {
    value: hir::ExprKind<'hir>,
}

/// The constructors we can compare values against. Miraculously, we only actually need integer
/// literals and records, since booleans and integers are represented as integers, and choice
/// values are represented as a record whose first entry is an integer tag. Furthermore, any bare
/// variables will have been moved over into the body of the arm as a binding.
enum Constructor {
    Integer(u32),
    Record(Vec<Constructor>),
}

/// The actual decision tree. At each step, we will compare a couple variables against a couple
/// constructors. If it matches, we head down one subtree, and if it fails we head down another.
/// If the list of variables and constructors is ever empty, then we are done and have
/// successfully matched. On the other hand, if the remaining list of arms is empty, then our
/// original set of arms must not have been exhaustive.
pub enum Decision<'hir> {
    Success(Body<'hir>),
    Failure,
    Branch {
        variables: Vec<InternedString>,
        constructors: Vec<Constructor>,
    },
}
