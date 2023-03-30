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
    let env = Env::new();

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

    let main = program
        .items
        .iter()
        .find(|item| item.name.literal == "main")
        .unwrap()
        .expr;
    let t = env.lower(&mut bindings, main, &mut errors).unwrap();
    println!("{:?}", env.typevars.borrow());
    println!("{t:#?}");
}

const PROG2: &str = r#"
    0 range 10
        map (|x| x + 1)
        filter (|x| x < 4)
        collect ()
        in |vec|
    
    vec print ()
"#;
