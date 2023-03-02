# Curse

This is a toy language roughly based on Rust, except every function is a binary operation called using infix notation.

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