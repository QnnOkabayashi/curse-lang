# Module System

The current "module" system is a temporary fix so we can write more interesting programs.
But we can do so much better!

I'm particularly inspired by the way Zig does things, where everything, even files, are just structs.
What I don't like is how each thing you import has to be a separate line. For example, I much prefer:
```rust
use std::{io, sync, mem};
```
to Zig's
```zig
const std = @import("std");
const io = std.io;
const sync = std.sync;
const mem = std.mem;
```
(I don't know if these modules exist in Zig, it's just translated 1-1 to Zig syntax)

My idea stems off of how pattern matching works. You know how in Rust, you can write:
```rust
let point = get_point();
let x = point.x;
let y = point.y;
```
But you can also pattern match to just say
```rust
let Point { x, y } = get_point();
```
I'm thinking we do pretty much the same thing in Curse for structs.
So if we have the following Rust import tree:
```rust
use std::{mem::{take, size_of}, io, sync};
```
Then it would be translated to Curse as:
```
{
    mem: { take, size_of },
    io,
    sync,
}: std,
```
Here, we "pattern match" on the `std` file to bring `take`, `size_of`, `sync`, and `io` into the scope of the existing file.

I really like this style because it keeps the Zig feel of "everything is a struct", but also allows for importing a lot of things in a tree structure like Rust does.

## More Examples

This could also apply to runtime code.
```
{
    { x: left, y: top }: { x: 0, y: 5 },
    { x: right, y: bottom }: { x: 4, y: 0 },
}
```

Is equivalent to
```
{
    left: 0,
    top: 5,
    right: 4,
    bottom: 0,
}
```


