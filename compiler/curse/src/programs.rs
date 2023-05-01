pub const FIB: &str = r#"
let fib: i32 () -> i32 = {
    |0| 0,
    |1| 1,
    |n| (n - 1 fib ()) + (n - 2 fib ())
}

let main: () () -> () = ||
    10 fib () print ()
"#;

pub const NESTED_CLOSURES: &str = r#"
let foo: i32 () -> i32 = {
    |0| 5 + 5 in {
        |10| 1 in |x| x + x,
        |_| 2,
    },
    |_| 3,
}
"#;

pub const NOT_EXHAUSTIVE: &str = r#"
let not_exhaustive: i32 () -> i32 = {
    |1| 1,
    |2| 2
}
"#;

pub const COND: &str = r#"
let int_of_bool: bool () -> i32 = {
    |false| 0,
    |true| 1
}
"#;

pub const NESTED_MATCHES: &str = r#"
let crazy: bool (bool, i32) -> i32 = {
    |true, (true, _)| 0,
    |true, (_, _)| 0,
    |_, (false, _)| 0,
    |_, (_, n)| 0
}
"#;

pub const TWICE: &str = r#"
let inc: i32 () -> i32 = |n|
    n + 1

let twice a: (a () -> a) a -> a = |f, x|
    x f () f ()

let main: () () -> () = ||
    inc twice 5 in print
"#;

pub const SUPERCHARGE: &str = r#"
let inc: i32 () -> i32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
let supercharge a: (a () -> a) () -> a () -> a = |f|
    |x| x f () f ()

let main: () () -> () = ||
    5 (inc in supercharge in supercharge) () print ()
"#;

pub const INVALID: &str = r#"
let in2 a b: a (a () -> b) -> b = |x, f|
    x f ()

let inc: i32 () -> i32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
let supercharge: (i32 () -> i32) () -> i32 () -> i32 = |f|
    |x| x f () f ()

let main: () () -> () = ||
    // should be:
    // 5 in (inc supercharge ()) in print
    inc supercharge () in2 5 in2 print
"#;

pub const ADDING: &str = r#"
let apply a b c: (a b -> c) (a, b) -> c = |f, (lhs, rhs)|
    lhs f rhs

let main: () () -> () = ||
    (+) apply (4, 5) in print
"#;

pub const IN2: &str = r#"
let of a b c: (a b -> c) (a, b) -> c = |f, (lhs, rhs)|
    lhs f rhs

let in2 a b: a (a () -> b) -> b = |x, f|
    x f ()

let main: () () -> () = ||
    5 in2 print
"#;

pub const SIMPLE: &str = r#"
let main: () () -> () = ||
    5 print ()
"#;

pub const MATH: &str = r#"
let main: () () -> () = ||
    5 in |x|
    4 in |y|
    x + y
"#;

pub const CLOSURE_MISMATCH_ARM_TYPES: &str = r#"
let fib: i32 () -> i32 =
    |a| 4 else
    |true| 5
"#;
