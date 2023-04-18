# Curse

Curse is a toy programming language inspired by the question: _How can we bend syntax to create the most readable, yet syntactically consistent programming language?_

For example, every function is a binary function called using infix notation.

To motivate this, think back to arithmatic: we don't write `+ 1 2`, we write `1 + 2`.
Despite what many LISP programmers argue, the ladder is far more natural for us to read.

In fact, the notion of chaining functions (evaluate, then pass to this function) is extremely common.
In Rust, we have method chaining, and some functional languages even go as far as adding a special pipe (`|>`) operator to do this exact functionality.


Another big difference is that there is only one way to write branching code: piecewise functions.
Piecewise functions are a combination of `match` expressions from functional languages, and functions.
This allows us to combine function definitions and all forms of branching into a single language construct, making the language smaller and feel more internally consistent.

The final difference is that there's no variable declaration inside of function bodies.
If you want to assign to a variable, you simply compute it and pass it into a function whose parameter has the name you want.

This is made more convenient using the builtin `in` function.

### Variables

Let's write a function that adds 4 and 5, and then adds the result to itself.

```rust
4 + 5 in |x|
x + x
```

Here, the `|x| x + x` is a closure exactly like in Rust.
This is roughly equivalent to the following Rust code:

```rust
let x = 4 + 5;
x + x
```

However, this is the direct 1-to-1 translation:

```rust
(|x| x + x)(4 + 5)
```

As such, there is no defining variables.
Instead, we just pass an expression into a function, somewhat similar to JavaScript's IIFEs (immediately invoked function calls).

Here are some more examples:
```rust
4 + 8 in |y|
5 in |x|
x + y
```

Every function in Curse takes has two parameters, and is called with infix notation.
```rust
0..20
    map (|x| x + 1)
    step_by 3
    filter (|x| x % 2 = 0)
    collect ()
    in |vec: Vec<i32>|

vec iter () for_each |value: &i32| {
    value in print
}
```

You may notice that we use `in` a lot.
This is just a function that takes a value and a function, and applies the value to the function and passes a `()` in as the second argument.
It is defined as follows:
```rust
fn in(x f) {
    x f ()
}
```

Example `main` function:
```rust
// If both inputs are `()`, they can be omitted in type
// If output is (), the `-> ()` can also be omitted.
fn main() {
    "Hello" in print
}
```

```rust
(|x f| x f ()) (|in| 
    4 + 5 in |x|
    x + x in print
) ()
```

Traditional `if-else` statements do not exist in Curse.
In fact, the only way to do branching is with piecewise closures.
```rust
true then (|| 5) else (|| 4)

a then (||
    1
) else (||
    b then (||
        2
    ) else (||
        3
    )
)

```
Here, `then` is similar to Rust's `bool::then` method which returns `Option<T>`, and `else` is like `Option::unwrap_or_else` method.
Right now, `else` conflicts with the builtin keyword for disambiguating closure arms, but I'll deal with that later.

This is what `map` in the above example would do internally:
```rust
cond in
    |Some(())| 5;
    |None| 4
```

```rust
params in 
    |Params::Zero| (left, right) in (
        |(Value::Tuple(t1), Value::Tuple(t2))| if t1.is_empty() && t2.is_empty() => Ok(()) else
        |_| Err(EvalError::TypeMismatch)
    ) else
    |Params::One(pat)| right in (
        |Value::Tuple(t)| if t.is_empty() => add_param_to_env(left, &pat, env) else
        |_| Err(EvalError::TypeMismatch)
    ) else
    |Params::Two(pat1, pat2)| (
        add_param_to_env(left, &pat1, env) and_then
        add_param_to_env(right, &pat2, env)
    )
```

```rust
// else should bind to the closest function
|1| (|2| 5 else |n| n) else
|x| |n| n + 2
```

Here's some ideas for top level functions
```rust
fn fib: i32 -> i32 =
    |0| 0 else
    |1| 1 else
    |n| n - 1 in fib + (n - 2 in fib)

fn main := ||
    10 in fib
```



