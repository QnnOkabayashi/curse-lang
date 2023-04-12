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

let twice a: (a () -> a) a -> a = |f, x|
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
let of A B C: (A B -> C) (A, B) -> C = |f, (a, b)|
    a f b

let in2 a b: a (a () -> b) -> b = |x, f|
    x f ()

let main: () () -> () = ||
    5 in2 print
"#;

const SIMPLE: &str = r#"
let main: () () -> () = ||
    5 print ()
"#;

const MATH: &str = r#"
let main: () () -> () = ||
    5 in |x|
    4 in |y|
    x + y
"#;

#[test]
fn test_branching_typeck() {
    let program = IN2;

    let ctx = curse_parse::Context::new();
    let program = curse_parse::parse_program(&ctx, program).unwrap();

    let counter = AllocationCounter::count_in_program(&program);

    println!("{counter:#?}");

    // TODO(quinn):
    // - Make types use a u32 ptr so we can reduce type size and put them in a vec
    //   so that we can put them in a vec and not have to reallocate them.
    //   Then you could do resolution and swap out the underlying arena of types
    //   without having to change the expression tree at all :)
    //   - Also make some abstraction so the allocation counter doesn't have to
    //     do its own tracking. Ideally we want it to just plug into some kind
    //     of visitor trait to keep things straightforward.
    // - Update to logos v0.13.0

    // Temporaries in struct literal constructors are allowed
    let mut env = Env {
        type_functions: &Arena::with_capacity(counter.type_functions),
        typevars: &mut Vec::new(),
        expr_appls: &Arena::with_capacity(counter.expr_appls),
        expr_branches: &Arena::with_capacity(counter.expr_branches),
        tuple_item_exprs: &Arena::with_capacity(counter.tuple_item_exprs),
        tuple_item_types: &Arena::with_capacity(counter.tuple_item_types),
        tuple_item_expr_pats: &Arena::with_capacity(counter.tuple_item_expr_pats),
        equations: &mut Equations::new(),
    };

    // temporary for now until we can have custom named types
    let type_scope: HashMap<&str, Type<'_>> = HashMap::new();

    let mut function_to_typescope: HashMap<&str, HashMap<&str, Type<'_>>> = HashMap::new();

    let globals: HashMap<&str, Polytype<'_>> = env
        .default_globals()
        .chain(program.items.iter().map(|item| {
            // Since items (i.e. functions for now) can be generic over types,
            // we need to extend the set of currently in-scope types with the
            // generics that this item introduces. To avoid bringing the types
            // into the global program scope, we'll create a temporary inner scope
            let mut inner_type_scope = type_scope.clone();

            let mut typevars = Vec::with_capacity(item.generics.len());
            inner_type_scope.reserve(item.generics.len());

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

    let mut locals: Vec<(&str, Type<'_>)> = Vec::with_capacity(16);
    let mut errors: Vec<LowerError> = Vec::with_capacity(0);

    let lowered_items: Result<HashMap<&str, (Polytype<'_>, Expr<'_, '_>)>, PushedErrors> = program
        .items
        .iter()
        .map(|item| {
            let item_name = item.name.literal;
            let mut scope = Scope::new(
                &mut env,
                &function_to_typescope[item_name],
                &mut errors,
                &globals,
                &mut locals,
            );

            let polytype = globals[item_name].clone();
            let body = scope.lower(item.expr)?;
            Ok((item_name, (polytype, body)))
        })
        .collect();

    println!("{}", env.type_functions.remaining_capacity());
    println!("{}", env.expr_appls.remaining_capacity());
    println!("{}", env.expr_branches.remaining_capacity());
    println!("{}", env.tuple_item_exprs.remaining_capacity());
    println!("{}", env.tuple_item_types.remaining_capacity());
    println!("{}", env.tuple_item_expr_pats.remaining_capacity());

    // Put the result into: https://edotor.net/
    // println!("{}", env.equations);

    if let Ok(lowered_items) = lowered_items {
        assert!(errors.is_empty());
        let mut builder = dot::Builder::new();
        for (_, expr) in lowered_items.values() {
            builder.visit_expr(*expr, None);
        }
        let out = builder.finish();
        println!("{out}");
    } else {
        assert!(!errors.is_empty());
        println!("Errors:");
        for (i, err) in errors.into_iter().enumerate() {
            println!("{i}) {err}");
        }
    }
}

const PROG2: &str = r#"
    0 range 10
        map (|x| x + 1)
        filter (|x| x < 4)
        collect ()
        in |vec|
    
    vec print ()
"#;
