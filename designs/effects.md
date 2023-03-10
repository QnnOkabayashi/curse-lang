# Effects

Effects offer ways to yield control flow to handlers in lower stack frames.
A common example is an exception-based throw-catch model, where a higher stack frame can throw an exception and it will be caught be a lower stack frame.
However, this idea isn't particularly exciting, since it's basically just short circuiting from a bunch of function calls.

Effects are particularly interesting because they provide the ability to resume execution from a stack frame that previously yielded.
When you generalize this notion, then you can create yielding generators, `async`/`await` models, and more in a general setting without special language support.

## How does this tie into IO?

Let's take an example straight from "A Gentle Introduction to Haskell":
```hs
getLine ::  IO string
getLine =   do c <- getChar
                if c == '\n'
                    then return ""
                    else do l <- getLine
                        return (c:l)
```

When we do this, we say "yield control to the IO provider, then once we get control back, do the rest of the expression."

In this sense, returning `IO string` isn't only saying `io::Result<String>`.
It also is a way of telling any callers "hey, you better also have an IO provider available if you want to call me".
That being said, I think it could still be an `io::Result<String>` under the hood, but with different semantics to the compiler.

Here's how we might define a similar operation in Rust:
```rs
fn get_line(provider: &IoProvider) -> io::Result<String> {
    let c: char = get_char(provider)?; // `?` is like the `do` operator here
    if c == '\n' {
        Ok("".to_string()) // Ok() is like Haskell's `return`
    } else {
        let l = get_line(provider)?;  // `?` is like the `do` operator here
        Ok(format!("{c}{l}")) // Ok() is like Haskell's `return`
    }
}
```

In order to use effects, it's also necessary to have an effect handler.
The analogy here is that without a `try-catch` block, you can't have a function that `throws`.

> Well, you can, but you're program just crashes.

## Practical Usages

* `'panics` requires a panic handler
* `'io` requires access to IO
* `'alloc` requires an allocator

## Lifetimes are effects

When you think about it, lifetimes in Rust are also effects.
A parameter of type `&'a T` is like an effect where the heap parameter is the lifetime `'a`, and the effect is `read` (since it's a shared reference).
In Koka terms, it's basically a `ref<'a, T>`

```kk
fun ref( value : a ) : (alloc<h>) ref<h, a>
fun set( ref : ref<h,a>, assigned : a ) : (write<h>) ()
fn (!)( ref : ref<h,a> ) : <read<h>|e> a
```

```rs
fn ref_<'a, T>(value: &'a T) -> &'a T;
fn set<'a, T>(ref_: &'a mut T, assigned: T) -> ();
fn bang<'a, T>(ref_: &'a T) -> T;
```

## Examples

```rs
fn map<'e, 'a> : ref<Bst<i32>>!['a, 'mut] (ref<i32>!['a, 'mut], () -> ()!['e]) -> ()!['e] =
    |(), f| () else
    |(k, v, l, r), f| (k, v in f, l map f, r map f) else
    |x, y| panic

// The effects are clearly indicated by the types
fn inc_and_print : i32 mut<'a>, () -> () = |x|
    // Somewhere below in the callstack, there's a handler whom I can call
    // get and set on and values will be produced and written respectively.
    x get () in |v: i32|
    // `;` is equivalent to `>>=`/`bind`/`and_then`
    // This should feel fairly natural since statements only exist
    // to produce effects.
    v in print; ||
    v + 1 in |v2: i32|
    v2 in print; ||
    v2 set x // `set` evals to `()`
```

And just like how Haskell needs `return`, we also need something to reconstruct
the type.
For this, we can use the name of the effect itself as the constructor, which the compiler automatically implements.
For example, the `io` effect would have a function with this signature.
```rs
fn io : T, () -> T io = /* magic */
```

# Inspirations

* Koka
* Eff
