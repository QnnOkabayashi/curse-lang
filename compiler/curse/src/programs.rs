#![allow(dead_code)]

// TODO(quinn): make HIR types have a little more resolution,
// such as knowing if a type struct/choice refers to a generic.

pub const TEST_ERR: &str = r#"
fn add = |x|
    4_294_967_295 in |東京|
    x + 東京

struct Wraps T = T (I32 * I32)

struct I32 = {}

struct List I32 = I32

struct Bool = {}

struct TGeneric T = T Bool

struct PrimParametric = I32 I32

struct Untyped = { name, age }

struct X = {
    age: I32,
}

struct X = {
    hot: Bool,
}
"#;

pub const SURPRISINGLY_OK: &str = r#"
struct X I32 = I32

struct I32 = Bool
"#;

pub const BINARY_TREE: &str = r#"
choice Option T =
    | Some T
    | None {}

choice Never = |

struct Pair (A * B) = { fst: A, snd: B }

struct LinearMap (K * V) = {
    data: Vec { key: K, value: V },
}

struct Wrapper T = T

struct Empty = {}

fn then_do = (
    |true, f| Some {} f {},
    |false, _| None {},
)

fn then = (
    |true, x| Some x,
    |false, _| None {},
)

fn else_do = (
    |Some val, _| val,
    |None {}, f| {} f {}
)

fn else = (
    |Some val, _| val,
    |None {}, x| x
)

choice Result (T * E) =
    | Ok T
    | Err E

choice Tree T =
    | Node {
        key: I32,
        value: T,
        left: Box Tree T,
        right: Box Tree T,
        thing: Vec Vec Result (I32 * Error),
    }
    | Empty {}

fn insert = (
    |Empty {}, { k, v }|
        Node { key: k, value: v, left: Empty {}, right: Empty {} },
    |Node { key, value, left, right }, { k, v }|
        k > key then_do (||
            right insert { k, v } in |right|
            Node { key, value, left, right }
        ) else_do || k < key then_do (||
            left insert { k, v } in |left|
            Node { key, value, left, right }
        ) else_do ||
            Node { key, value: v, left, right }
)

fn get = (
    |Empty {}, _|
        None {},
    |Node { key, value, left, right }, k|
        k > key then_do (||
            right find k
        ) else_do || k < key then_do (||
            left find k
        ) else_do ||
            Some value
)

// let rec fix f x = f (fix f) x (* note the extra x; here fix f = \x-> f (fix f) x *)
// 
// let factabs fact = function   (* factabs has extra level of lambda abstraction *)
//    0 -> 1
//  | x -> x * fact (x-1)
// 
// let _ = (fix factabs) 5

// Y-combinator (works in strict languages)
fn rec = |x, f| f of (|x| x fix f) of x

// Tail-recursive factorial function
fn fact = |n|
    { acc: 1, n } rec |loop| (
        |{ acc, n: 1 }| acc,
        |{ acc, n }| loop of { acc: acc * n, n: n - 1 },
    )

fn print_0_to_n = |n, io|
    0 rec |loop| (
        |10| 10 println io,
        |i|
            i println io;
            loop of (i + 1)
    )

fn in = |x, f| f of x

fn of = |f, x| x f {}
"#;

pub const FIB: &str = r#"
choice Option T = Some T | None {}

fn fib = (
    |0| 0,
    |1| 1,
    |n| (n - 1 fib {}) + (n - 2 fib {})
)

fn main = || 10 fib {} print {}
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
    |true| 1,
    |false| 0
}
"#;

pub const NESTED_MATCHES: &str = r#"
fn crazy: Bool (Bool, I32) -> i32 = {
    |true, (true, _)| 0,
    |true, (_, _)| 0,
    |_, (false, _)| 0,
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
    |true| 5
"#;
