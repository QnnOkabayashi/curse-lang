// choice variants either have 0 or 1 values
// variants are labeled with a single quote
// at the beginning
pub const CHOICE_TYPES: &str = r#"
choice Option T {
    'some T,
    'none,
}

// fn print_if_some: Option T, () -> () = {
//     |'some v| v in print,
//     |'none| (),
// }
"#;

pub const FIB: &str = r#"
fn fib: i32 () -> i32 = {
    |0| 0,
    |1| 1,
    |n| (n - 1 fib ()) + (n - 2 fib ())
}

fn main: () () -> () = ||
    10 fib () print ()
"#;

pub const NESTED_CLOSURES: &str = r#"
fn foo: i32 () -> i32 = {
    |0| 5 + 5 in {
        |10| 1 in |x| x + x,
        |_| 2,
    },
    |_| 3,
}
"#;

pub const NOT_EXHAUSTIVE: &str = r#"
fn not_exhaustive: i32 () -> i32 = {
    |1| 1,
    |2| 2
}
"#;

pub const COND: &str = r#"
fn int_of_bool: bool () -> i32 = {
    |true| 1,
    |false| 0
}
"#;

pub const NESTED_MATCHES: &str = r#"
fn crazy: bool (bool, i32) -> i32 = {
    |true, (true, _)| 0,
    |true, (_, _)| 0,
    |_, (false, _)| 0,
    |_, (_, n)| 0
}
"#;

pub const TWICE: &str = r#"
fn inc: i32 () -> i32 = |n|
    n + 1

fn twice a: (a () -> a) a -> a = |f, x|
    x f () f ()

fn main: () () -> () = ||
    inc twice 5 in print
"#;

pub const SUPERCHARGE: &str = r#"
fn inc: i32 () -> i32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
fn supercharge a: (a () -> a) () -> a () -> a = |f|
    |x| x f () f ()

fn main: () () -> () = ||
    5 (inc in supercharge in supercharge) () print ()
"#;

pub const INVALID: &str = r#"
fn in2 a b: a (a () -> b) -> b = |x, f|
    x f ()

fn inc: i32 () -> i32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
fn supercharge: (i32 () -> i32) () -> i32 () -> i32 = |f|
    |x| x f () f ()

fn main: () () -> () = ||
    // should be:
    // 5 in (inc supercharge ()) in print
    inc supercharge () in2 5 in2 print
"#;

pub const ADDING: &str = r#"
fn apply a b c: (a b -> c) (a, b) -> c = |f, (lhs, rhs)|
    lhs f rhs

fn main: () () -> () = ||
    (+) apply (4, 5) in print
"#;

pub const IN2: &str = r#"
fn of a b c: (a b -> c) (a, b) -> c = |f, (lhs, rhs)|
    lhs f rhs

fn in2 a b: a (a () -> b) -> b = |x, f|
    x f ()

fn main: () () -> () = ||
    5 in2 print
"#;

pub const SIMPLE: &str = r#"
fn main: () () -> () = ||
    5 print ()
"#;

pub const MATH: &str = r#"
fn main: () () -> () = ||
    5 in |x|
    4 in |y|
    x + y print ()
"#;

pub const CLOSURE_MISMATCH_ARM_TYPES: &str = r#"
fn fib: i32 () -> i32 =
    |a| 4 else
    |true| 5
"#;
