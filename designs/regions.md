# Regions

WORK IN PROGRESS

## References

Rust tells us that as long as there's a reference to a value that's in use, the original value should not be touched.

So how do you prevent a user from doing that?
Rust's approach is to simply raise a compiler error telling you what you did wrong.
But this can be confusing because people might not understand why they're wrong.

A better approach is to force references to shadow the value they reference, making it impossible for the programmer to even refer by name to the original value.

```rust
let x = Foo::default();
let x_ref = &x;

drop(x); // compiler error - x_ref still used!

do_stuff(x_ref);
```

What if Rust forced you to write like this though?
```rust
let x = Foo::default();
{
    let x = &x;

    // can't refer to `x` in outer scope
    // previous mistake is impossible
    drop(???);

    do_stuff(x);
}
```

For Curse, I propose the following syntax:
```curse
ref <IDENT> {
    <EXPR>
}
```

Here, `<IDENT>` is an identifier of a binding that's in scope.
Then for the duration of the region, `<EXPR>`, `<IDENT>` is a binding to a reference to the value that `<IDENT>` binds in the outer scope.

The whole expression evaluates to whatever `<EXPR>` evaluates to, and must not contain any references to `<IDENT>` in any way.
Presumably this would be implemented through a borrow checker similar to Rust.

## Mutation

There are two kinds of mutation in programming: writing to a value, and writing to a pointer.

This is evident by the fact that the assembly for each is fundamentally different:
```asm
# write 1 to %eax
movl $1, %eax

# write 1 to addr in %eax
movl $1, (%eax)
```

And yet, languages that support the notion of pointers or references seem to want to hide this fact.
Take C for example:
```c
// write to `x``
int x = ...;
int *ptr = ...;

x = 1;

// write to addr in `ptr`
*ptr = 1;
```

When you squint your eyes a little, this example kind of makes sense.
We know that `*ptr` dereferences the pointer and gives the underlying value, so getting that value and reassigning it makes sense.

Except that that's not actually what's going on.
Because if it worked this way, how does the value produced by `*ptr` "remember" that it lived behind `ptr`?
It can't possibly do that, which is evident from the following C program:

```c
int x = 0;
int* ptr = &x;

// dereferencing creates a copy
int value_behind_ptr = *ptr;
value_behind_ptr = 1;

// prints 0
printf("%d\n", x);
```

If you simply copy paste the definition of `value_behind_ptr` into its singular use case, the semantics of the program are different.

Simply put, this means that when you have `*ptr` in an expression, it dereferences and creates a copy of the value behind a pointer.
But when you have `*ptr` in the left-hand side of an assignment, no such copy is made: it's actually telling the compiler "we're going to do the special 'write to an address' asm instruction here."

I'm pretty sure this is what lvalues and rvalues are all about, but I can't confirm because I'm too lazy to read into them and all their complexity.

## Making Mutation Consistent

You know what makes sense?
Always writing to a pointer.
It's dead simple to understand the semantics of, and you can always write to the pointer of a local binding if you just want to update a value.

Want to increment a local binding?
Create a reference region to it and write to the reference.
No more lvalue and rvalue distinction, only references.

So in Curse, we should just expose a way to write to references, with certain patterns guaranteed to get optimized into just writing by value.
As showned above, we have syntax for getting a reference to a value for the duration of a region, but what syntax should we use for writing to a reference?

## Mutation with better syntax

One idea that I was toying around with was linear typing, meaning you can force the programmer to handle a variable instead of just forgetting about it.

Although I'm not fixated on this idea, I do think it's an interesting option and we shouldn't block ourselves off from it.

Where mutation comes in is that assignment involves overwriting a value.
In Rust, assignment invokes `Drop` on the old value, which is one solution, but doesn't carry over to linear typing very well.
As such, we need a way to update the value of a binding while also handling the old value.

What if all mutation, instead of "assignment", was an "update"?
Some way to take a binding, and map the underlying value to a new value that will go into the binding?

That way, all values that we want to get rid of must be handled, and we can even use an old value to compute a new value.

For this, I propose another kind of region: `mut`.
It has the same syntax as a `ref` region (for now), but inside the region the identifier is binded to the underlying value, making it impossible to refer to the reference to the value, and the result of the body is the new value assigned to the binding.

```curse
0 in |x|
ref x {
    mut x {
        x + 1
    }
}
```

You can almost think of the above program as translating to the following Rust program, where `mutate` is built into the compiler:
```rust
fn mutate<T, F>(ptr: &mut T, f: F)
where
    F: FnOnce(T) -> T,
{
    unsafe {
        let value = ptr::read(ptr);
        let updated_value = f(value);
        ptr::write(updated_value)
    }
}

let mut x = 0;
{
    let x = &mut x;
    mutate(x, |x| x + 1);
}
```

One interesting thing is that in the Curse program, mutating the value behind a reference requires shadowing the reference itself, meaning that while you're computing the new value, you cannot even refer to the binding that contains the reference.

This is _essential_, since at a bits level, it allows for us to move the value from behind the reference into the scope of the region, and then not have to worry about the reference being used within the region.

Here are some further examples:
```curse
// We don't have to use `x` in the update
0 in |x|
ref x {
    mut x {
        1
    }
} in ||

// evals to 1
x
```