use super::*;

#[test]
fn test_branching_typeck() {
    const PROGRAM: &str = r#"
let fib : i32 () -> i32 =
    |0| 0 else
    |1| 1 else
    |n| n - 1 fib () + (n - 2 fib ())

let main : () () -> () = ||
    10 fib () print ()"#;

    let ctx = curse_parse::Context::new();
    let program = curse_parse::parse_program(&ctx, PROGRAM).unwrap();

    let types = Arena::new();
    let mut typevars = Vec::new();
    let exprs = Arena::new();
    let expr_pats = Arena::new();
    let expr_branches = Arena::new();
    let mut equations = Equations::new();

    let mut env = Env::new(
        &types,
        &mut typevars,
        &exprs,
        &expr_pats,
        &expr_branches,
        &mut equations,
    );

    let mut globals = env.default_globals();
    globals.extend(program.items.iter().map(|item| {
        (
            item.name.literal.to_string(),
            Polytype {
                typevars: vec![],
                typ: env.type_from_ast(item.typ),
            },
        )
    }));

    let mut locals = Vec::with_capacity(16);
    let mut bindings = Bindings::new(&globals, &mut locals);

    let mut errors = vec![];

    let fib = program
        .items
        .iter()
        .find(|item| item.name.literal == "fib")
        .unwrap()
        .expr;
    let _t = env.lower(&mut bindings, fib, &mut errors).unwrap();
    // Put the result into: https://dreampuf.github.io/GraphvizOnline/
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
