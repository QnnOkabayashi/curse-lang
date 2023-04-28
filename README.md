# Curse

Curse is a statically-typed toy programming language inspired by the question: _How can we bend syntax to create the most readable, yet syntactically consistent programming language?_

Currently, it's founded on two core ideas.

## 1. Function chaining should be natural

For example, every function is a binary function called using infix notation.

To motivate this, think back to arithmatic: we don't write `+ 1 2`, we write `1 + 2`.
Despite what many LISP programmers argue, the ladder is far more natural for us to read.

In fact, the notion of chaining functions (evaluate, then pass to this function) is extremely common.
In Rust, we have method chaining, and some functional languages even go as far as adding a special pipe (`|>`) operator to do this exact functionality.

## 2. A single branching primitive

Another big difference is that there is only one way to write branching code: piecewise functions.
Piecewise functions are a combination of `match` expressions from functional languages, and functions.
This allows us to combine function definitions and all forms of branching into a single language construct, making the language smaller and feel more internally consistent.

Piecewise closures are statically checked, meaning that the compiler is able to determine when a piecewise closure is exhaustive and when all branches are useful.

The final difference is that there's no variable declaration inside of function bodies.
If you want to assign to a variable, you simply compute it and pass it into a function whose parameter has the name you want.

This is made more convenient using the builtin `in` function.

___

Let's take a little tour of the language.

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
let in a b: a (a () -> b) -> b |x, f| x f ()
```

Example `main` function:
```rust
let main: () () -> () = ||
    "Hello" in print
```


Traditional `if-else` statements do not exist in Curse.
In fact, the only way to do branching is with piecewise closures.

As an example, let's examine the `fib` function:
```rust
let fib: i32 () -> i32 = {
    |0| 0,
    |1| 1,
    |n| (n - 1 in fib) + (n - 2 in fib)
}
```

Do make a closure piecewise, it's necessary to wrap the branches in curly braces.
This means if you ever see the start of a closure and it's not wrapped in braces, you can be sure that it doesn't branch.
This means it's also easy to visually tell which closure arms below to which closure, even in the case of nested piecewise closures:

```rust
{
    |1| {
        |2| 5,
        |n| n
    },
    |x| |n| n + 2
}
```

Using this construct, we can also encode `if-else` expressions by matching on `true` and `false`.
In fact, once we have variant types, we intend to create a `then` function, which has the signature `bool (() () -> a) -> Option a`, as well as an `else` function with the signature `(Option a) (() () -> a) -> a`.
In a sense, `then` will be like `bool::then` in Rust, and `else` will be like `Option::unwrap_or_else`.

