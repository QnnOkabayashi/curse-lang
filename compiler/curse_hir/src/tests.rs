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
    let program = MATH;

    let ctx = curse_parse::Context::new();
    let program = curse_parse::parse_program(&ctx, program).unwrap();
    let counter = AllocationCounter::count_in_program(&program);

    let mut typevars = Vec::new();
    let types: Arena<Type> = Arena::with_capacity(1024); // need to precompute this..
    let exprs: Arena<Expr> = Arena::with_capacity(counter.num_exprs);
    let expr_pats: Arena<ExprPat> = Arena::with_capacity(counter.num_expr_pats);
    let expr_branches: Arena<List<ExprBranch>> = Arena::with_capacity(counter.num_branches);
    let tuple_item_exprs: Arena<List<&Expr>> = Arena::with_capacity(counter.num_tuple_item_exprs);
    let tuple_item_types: Arena<List<&Type>> = Arena::with_capacity(counter.num_tuple_item_exprs);
    let tuple_item_expr_pats: Arena<List<&ExprPat>> = Arena::with_capacity(1024);
    let mut equations: Equations = Equations::new();

    let mut env = Env {
        types: &types,
        typevars: &mut typevars,
        exprs: &exprs,
        tuple_item_exprs: &tuple_item_exprs,
        tuple_item_types: &tuple_item_types,
        tuple_item_expr_pats: &tuple_item_expr_pats,
        expr_pats: &expr_pats,
        expr_branches: &expr_branches,
        equations: &mut equations,
    };

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

    let mut locals: Vec<(&str, &Type<'_>)> = Vec::with_capacity(16);
    let mut errors: Vec<LowerError> = Vec::with_capacity(0);

    let lowered_items: Result<HashMap<&str, (Polytype<'_>, &Expr<'_, '_>)>, PushedErrors> = program
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

    // Put the result into: https://edotor.net/
    // println!("{}", env.equations);

    // assert_eq!(exprs.remaining_capacity(), 0);
    // assert_eq!(expr_pats.remaining_capacity(), 0);
    // assert_eq!(expr_branches.remaining_capacity(), 0);

    if let Ok(items) = lowered_items {
        let expr = items["main"].1;
        assert!(errors.is_empty());
        // println!("{expr}");
        let mut out = vec![];
        dot::render(&Graph::new(expr), &mut out).unwrap();
        let out = String::from_utf8(out).unwrap();
        println!("{out}");
    } else {
        assert!(!errors.is_empty());
        println!("Errors:");
        for (i, err) in errors.into_iter().enumerate() {
            println!("{i}) {err}");
        }
    }
}

// #[test]
fn test_count_allocations() {
    let program = r#"
let main: () () -> () = ||
    5 in |x|
    4 in |y|
    x + y
"#;

    let ctx = curse_parse::Context::new();
    let program = curse_parse::parse_program(&ctx, program).unwrap();

    let counter = AllocationCounter::count_in_program(&program);
    assert_eq!(counter.num_exprs, 13);
    assert_eq!(counter.num_expr_pats, 2);
    assert_eq!(counter.num_branches, 3);
}

const PROG2: &str = r#"
    0 range 10
        map (|x| x + 1)
        filter (|x| x < 4)
        collect ()
        in |vec|
    
    vec print ()
"#;
