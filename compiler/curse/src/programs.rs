#![allow(dead_code)]
// choice variants either have 0 or 1 values
// variants are labeled with a single quote
// at the beginning
pub const CHOICE_TYPES: &str = r#"
choice Option T {
    Some T,
    None,
}

// fn print_if_some: Option T, () -> () = {
//     |Some v| v in print,
//     |None| (),
// }
"#;

pub const BINARY_TREE: &str = r#"
choice Option T {
    Some T,
    None (),
}

fn then {
    |True, f| Some (() f ()),
    |False, _| None
}

fn else {
    |Some val, _| Some val,
    |None, f| () f ()
}

choice Tree T {
    Node (I32, T, Tree T, Tree T),
    Empty (),
}

fn insert {
    |Empty (), (k, v)|
        Node (k, v, Empty (), Empty ()),
    |Node (key, value, left, right), (k, v)|
        k > key then (||
            Node (key, value, left, right insert (k, v))
        ) else || k < key then (||
            Node (key, value, left insert (k, v), right)
        ) else ||
            Node (key, v, left, right)
}

fn get {
    |Empty (), _|
        None,
    |Node (key, value, left, right), k|
        k > key then (||
            right find k
        ) else || k < key then (||
            left find k
        ) else ||
            Some value
}
"#;

pub const FIB: &str = r#"
fn fib: I32 () -> I32 = {
    |0| 0,
    |1| 1,
    |n| (n - 1 fib ()) + (n - 2 fib ())
}

fn main: () () -> () = ||
    10 fib () print ()
"#;

pub const NESTED_CLOSURES: &str = r#"
fn foo: I32 () -> I32 = {
    |0| 5 + 5 in {
        |10| 1 in |x| x + x,
        |_| 2,
    },
    |_| 3,
}
"#;

pub const NOT_EXHAUSTIVE: &str = r#"
fn not_exhaustive: I32 () -> i32 = {
    |1| 1,
    |2| 2
}
"#;

pub const COND: &str = r#"
fn int_of_bool: bool () -> I32 = {
    |True| 1,
    |False| 0
}
"#;

pub const NESTED_MATCHES: &str = r#"
fn crazy: Bool (Bool, I32) -> i32 = {
    |True, (True, _)| 0,
    |True, (_, _)| 0,
    |_, (False, _)| 0,
    |_, (_, n)| 0
}
"#;

pub const TWICE: &str = r#"
fn inc: I32 () -> i32 = |n|
    n + 1

fn twice T: (T () -> T) T -> T = |f, x|
    x f () f ()

fn main: () () -> () = ||
    inc twice 5 in print
"#;

pub const SUPERCHARGE: &str = r#"
fn inc: I32 () -> I32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
fn supercharge T: (T () -> T) () -> T () -> T = |f|
    |x| x f () f ()

fn main: () () -> () = ||
    5 (inc in supercharge in supercharge) () print ()
"#;

pub const INVALID: &str = r#"
fn in2 A B: A (A () -> B) -> B = |x, f|
    x f ()

fn inc: I32 () -> I32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
fn supercharge: (I32 () -> I32) () -> I32 () -> I32 = |f|
    |x| x f () f ()

fn main: () () -> () = ||
    // should be:
    // 5 in (inc supercharge ()) in print
    inc supercharge () in2 5 in2 print
"#;

pub const ADDING: &str = r#"
fn apply A B C: (A B -> C) (A, B) -> C = |f, (lhs, rhs)|
    lhs f rhs

fn main: () () -> () = ||
    (+) apply (4, 5) in print
"#;

pub const IN2: &str = r#"
fn of A B C: (A B -> C) (A, B) -> C = |f, (lhs, rhs)|
    lhs f rhs

fn in2 A B: A (A () -> B) -> B = |x, f|
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
fn fib: I32 () -> I32 =
    |a| 4,
    |True| 5
"#;
