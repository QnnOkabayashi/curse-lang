use super::*;

const FIB: &str = r#"
let fib : i32 () -> i32 =
    |0| 0 else
    |1| 1 else
    |n| n - 1 fib () + (n - 2 fib ())

let main : () () -> () = ||
    10 fib () print ()
"#;

const TWICE: &str = r#"
let inc: i32 () -> i32 = |n|
    n + 1

let twice: (i32 () -> i32) i32 -> i32 = |f, x|
    x f () f ()

let main: () () -> () = ||
    inc twice 5 in print
"#;

const SUPERCHARGE: &str = r#"
let inc: i32 () -> i32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
let supercharge: (i32 () -> i32) () -> i32 () -> i32 = |f|
    |x| x f () f ()

let main: () () -> () = ||
    5 in (inc supercharge ()) in print
"#;

const INVALID: &str = r#"
let inc: i32 () -> i32 = |n|
    n + 1

// Given a function, return a function thats like it but calls the provided fn twice!
let supercharge: (i32 () -> i32) () -> i32 () -> i32 = |f|
    |x| x f () f ()

let main: () () -> () = ||
    // should be:
    // 5 in (inc supercharge ()) in print
    inc supercharge () in 5 in print
"#;

const ADDING: &str = r#"
let apply: (i32 i32 -> i32) (i32, i32) -> i32 = |f, (a, b)|
    a f b

let main: () () -> () = ||
    (+) apply (4, 5) in print
"#;

#[test]
fn test_branching_typeck() {
    let program = SUPERCHARGE;

    let ctx = curse_parse::Context::new();
    let program = curse_parse::parse_program(&ctx, program).unwrap();

    let mut allocations = Allocations::default();
    let mut env = Env::new(&mut allocations);

    let globals = env
        .default_globals()
        .chain(program.items.iter().map(|item| {
            (
                item.name.literal.to_string(),
                Polytype {
                    typevars: vec![],
                    typ: env.type_from_ast(item.typ),
                },
            )
        }))
        .collect();

    let mut locals = Vec::with_capacity(16);
    let mut bindings = Bindings::new(&globals, &mut locals);

    let mut errors = vec![];

    let main = program
        .items
        .iter()
        .find(|item| item.name.literal == "main")
        .unwrap()
        .expr;

    let expr = env.lower(&mut bindings, main, &mut errors);
    if errors.is_empty() {
        // success
        let _ = expr;
    } else {
        println!("failed");
    }
    // Put the result into: https://edotor.net/
    println!("{}", env.equations);

    // println!("{:?}", env.typevars.borrow());
    // println!("{t:#?}");
}

const PROG2: &str = r#"
    0 range 10
        map (|x| x + 1)
        filter (|x| x < 4)
        collect ()
        in |vec|
    
    vec print ()
"#;
