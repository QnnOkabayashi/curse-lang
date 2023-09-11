# Generics Syntax

WORK IN PROGRESS

Key differences:
* No more `=`.
* Generics are now in pipes, making generic types feel more akin to kinds from type theory.

```curse
// No generics
struct Point {
    x: I32,
    y: I32,
}

// struct Id(i32);
struct Id I32

// struct Wrapper<T>(T);
struct Wrapper |T| T

// struct TaggedWrapper<T> { .. }
struct TaggedWrapper |T| {
    value: T,
    yes: Bool,
}

// struct Pair<A, V>(A, B);
struct Pair |A * B| (A, B)

// struct Map<K, V>(Vec<Pair<K, V>>);
struct Map |K * V| Vec Pair (K * V)

choice Color {
    Red {},
    Green {},
    Blue {},
}

// enum Option<T> { .. }
choice Option |T| {
    Some T,
    None {},
}

// no more `=`
fn add |a, b| {
    a + b
}
```