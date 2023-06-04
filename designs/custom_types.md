## Problem Statement

It would be nice to allow for the never type (`choice Never {}`) and the function that accepts it and its isomorphisms, the empty piecewise function (denoted as `{}`).
The problem is that this introduces syntactic ambiguity when an expression like `MyType {} ...` arises: is it a constructor with no named fields, or a newtype around a never function?

## A chain of proposals

Here are this list of changes proposed:
1. make piecewise functions be wrapped in parentheses,
2. replace tuples with records, anonymous types that are sets of named fields,
3. make structs and choice variants all newtypes, only capable of carrying 1 value,
4. new type and generic syntax.

## Piecewise function syntax

Instead of writing this:
```
{
    |true| 1,
    |false| 0,
}
```

You would instead write this:
```
(
    |true| 1,
    |false| 0,
)
```
This is pretty straightforward.

## Replacing Tuples with Records

Everything that a tuple can do, a record can do better, especially with the help of a little syntactic sugar during construction/destructuring.

That's right, we're hopping on the Typescript/Zig train 😎

This also means that functions that take a lot of arguments now get named arguments for free, which is very good and is what Swift has been doing for years too.

Most importantly, this along with the above proposed change for piecewise function syntax mean that parenthesis are exclusively for functions and braces are exclusively for data.
So `()` is the empty piecewise function and `{}` is the empty record.

## Only newtypes allowed

Now that we've reserved braces for records, we can also make structs and choice variants only be newtypes, since they can wrap records now.
There's a lot more thought to be put in here but it's a start.

## New Type and Generics Syntax

For clarity, I propose that struct definitions have syntax like the following, with the `=` sign:
```
struct Point = { x: I32, y: I32 }
```

Choice types should be separated by pipes like ML languages, with an optional leading pipe:
```
choice Color = Red {} | Green {} | Blue {}
```

For generics, a single generic parameter can go between the type name and the `=`:
```
choice Option T = Some T | None {}
```

For multiple generics, we instead write the cartesian product:
```
struct LinearMap (K * V) = Vec { key: K, value: V }
```
Here, the inner value is a `Vec` of records.

Here are some example types you might write:
```
// Rust: Option<Option<i32>>
Option Option I32

// Rust: Result<i32, Error>
Result (I32 * Error)

// Rust: Vec<LinearMap<I32, Vec<Error>>>
Vec LinearMap (I32 * Vec Error)
```
Hopefully, these comparisons to Rust syntax make it clear why we'd want this terser syntax.

