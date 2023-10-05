# Fields as Patterns

This design proposal deals with syntax for _constructing_ sets of unordered key-value pairs, aka structs, objects, or according to Curse/PL theory, records.

Records are pretty boring: they consist of identifier keys and then some values.
```js
{
    x: 4,
    y: 5,
    nested: {
        foo: 
    }
}
```

Notably, there's only one way to construct records: by literally writing out each field name followed by the value that the field binds to.

On the other hand, traditional runtime code has more than one way to create bindings to values.
```rust
let (x, y) = (4, 5);
```
as opposed to

```rust
let x = 4;
let y = 5;
```
> Notably, I'm not making an argument for which is better. Just pointing out that there's multiple ways.

In both examples, `x` and `y` are brought into scope.
In fact, Rust allows for arbitrary patterns to be used in `let` statements, as long as they're irrefutable.

Why not have the same for records?
What if we could write:
```
{
    { x, y }: { x: 3, y: 4 },
}
```
Which results in the record:
```
{
    x: 3,
    y: 4,
}
```

Essentially, in record constructors, we should allow for the binding to not just be an identifier, but an irrefutable pattern where each binding created is a field in the record.


## Renaming

In the following Rust example, we pattern match on a value and bind the fields `x` and `y` to bindings `width` and `height` respectively:
```rust
let Vec2D { x: width, y: height } = get_vec2d();
```

For record constructors, why not be able to do the same?
```
{
    { x: width, y: height }: { x: 3, y: 4 },
}
```
is the same as
```
{
    width: 3,
    height: 4,
}
```


## View types

One idea that's particularly interesting to me are view types, which allow for only having access to part of a type.

With fields as patterns, it gives a concise way to say "create a binding to only this subset of fields of a record"

```
{ a: 1, b: 2, c: 3 } in |{ { a, b }: a_b_view, c }| ...
```

This example so far would just take `a` and `b` by value, but you can imagine how the syntax is set up nicely by this for when we do have references in the future.


## Imports

If we treat Curse files as a record type, similar to Zig, then we can have really nice tree imports too:
```
{
    mem: { take, size_of },
    io,
    sync,
}: std,
```
Here, `take`, `size_of`, `io`, and `sync` are all brought into the top scope.

Again, a lot of the details around imports haven't been thought out yet, but the syntax introduced in the proposal offers a lot of flexibility and clarity around imports.


## Does this change what records are?

No, records are still key-value pairs.
This proposal just suggests new possible syntaxes on how to construct them.

