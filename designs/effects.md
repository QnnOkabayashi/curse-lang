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
effect io {
    fn print: T, () -> io ();
}

// need syntax for an effectful function that is a struct
// values "on the stack" are actually in the struct.

// The effects are clearly indicated by the types
fn inc_and_print : i32 mut, () -> io () = |x|
    // Somewhere below in the callstack, there's a handler whom I can call
    // get and set on and values will be produced and written respectively.
    x get () in |v: i32|
    // when calling print, we get a special type that is capable of being yielded,
    // kinda like a result. Then, `;` is a binary operator that takes a yieldable
    // value and yields it, and also passes in the next closure to be resumed.
    // Still need to think about how this would compose...
    v in print; ||
    v + 1 in |v2: i32|
    v2 in print; ||
    v2 set x // `set` evals to `()`

fn echo : () () -> io () = ||
    () in String::new in |buf: String|
    () in io::stdin read_line buf mut; |num: usize|
    num in print; ||
    buf in print; ||
    ()
```

After discussion with William, we came across the idea: what if every effect in Curse was like how `async`/`await` works in Rust?
That is, across every "await" point, the program yields to its effect handler, which would be equivalent to an async scheduler.
Obviously this would be ridiculous if we still considered mutable references like effects, but it does work particularly well with iterators and async code.
However, you wouldn't really want to resume after panicking.
But it could be nice to allocate and then resume, or do IO and then resume.

One benefit of this is that all pure functions would be parallelizable by default.

## Plan

All pure functions are just that- pure.
When you call it, it creates a new stack frame on top of the stack and performs as usual.

On the other hand, effectful functions are state machines.
Effects include:
* `async`/`await`
* panicking
* IO
* mutation (?)
* nondeterminism
* or any other globally-accessed resource.

```rust
async fn do_it() {
    let a = do_a().await;
    let b = do_b(a).await;
    let c = do_c(b).await;
    c
}

fn do_it : io e |e |x| y| y = |do_c|
    () in do_a; |a|
    a in do_b; |b|
    b in do_c
```

# Inspirations

* Koka
* Eff
