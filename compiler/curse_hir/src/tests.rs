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
    5 (inc supercharge ()) () print ()
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
let apply a b c: (a b -> c) (a, b) -> c = |f, (lhs, rhs)|
    lhs f rhs

let main: () () -> () = ||
    (+) apply (4, 5) in print
"#;

const IN2: &str = r#"
let in2 a b: a (a () -> b) -> b = |x, f|
    x f ()

let main: () () -> () = ||
    5 in2 print
"#;

#[test]
fn test_branching_typeck() {
    let program = SUPERCHARGE;

    let ctx = curse_parse::Context::new();
    let program = curse_parse::parse_program(&ctx, program).unwrap();

    let mut allocations = Allocations::default();
    let mut env = Env::new(&mut allocations);

    // temporary for now until we can have custom named types
    let type_scope: HashMap<&str, &Type<'_>> = HashMap::new();

    let mut function_to_typescope: HashMap<&str, HashMap<&str, &Type<'_>>> = HashMap::new();

    let globals: HashMap<&str, Polytype<'_>> = env
        .default_globals()
        .chain(program.items.iter().map(|item| {
            // Since items (i.e. functions for now) can be generic over types,
            // we need to extend the set of currently in-scope types with the
            // generics that this item introduces. To avoid bringing the types
            // into the global program scope, we'll create a temporary inner scope
            let mut inner_type_scope = type_scope.clone();

            let mut typevars = Vec::with_capacity(item.generics.len());

            for generic in item.generics.iter() {
                let (var, ty) = env.new_typevar();
                typevars.push(var);
                inner_type_scope.insert(generic.literal, ty);
            }

            let typ = env.type_from_ast(item.typ, &inner_type_scope);

            function_to_typescope.insert(item.name.literal, inner_type_scope);

            (item.name.literal, Polytype { typevars, typ })
        }))
        .collect();

    let mut locals: Vec<(&str, &Type<'_>)> = Vec::with_capacity(16);
    let mut errors: Vec<LowerError> = Vec::with_capacity(0);
    let _lowered_items: HashMap<&str, (Polytype<'_>, Option<&Expr<'_, '_>>)> = program
        .items
        .iter()
        .map(|item| {
            let item_name = item.name.literal;
            let polytype = globals[item_name].clone();
            let body = env.lower(
                &mut Bindings::new(&globals, &mut locals),
                item.expr,
                &function_to_typescope[item_name],
                &mut errors,
            );
            (item_name, (polytype, body))
        })
        .collect();

    for e in errors {
        println!("{e}");
    }

    // Put the result into: https://edotor.net/
    println!("{}", env.equations);
    println!("{:#?}", _lowered_items["main"].1.as_ref().unwrap());
}

const PROG2: &str = r#"
    0 range 10
        map (|x| x + 1)
        filter (|x| x < 4)
        collect ()
        in |vec|
    
    vec print ()
"#;
