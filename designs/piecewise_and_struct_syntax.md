# Piecewise and Struct Syntax

## Background

This document contains a list of several changes that I (Quinn) want to make to the Curse syntax.
The [Proposed Changes](#proposed-changes) section summarizes all the changes.
The [Motivation](#motivation) section outlines my thought process for each change and why it's necessary.


## Proposed Changes

* Add tuples again.
* Piecewise syntax should be changed to use the `choice` keyword, followed by comma-delimited arms that are enclosed in braces.
  ```curse
  choice {
      |0| false,
      |_| true,
  }
  ```
* Braces expressions should be added, which semantically are the same as parenthesized expressions.
* Top level functions are requred to have their bodies enclosed in braces.
* Records (aka anonymous structs) should require the `struct` keyword in construction and deconstruction.
  ```curse
  struct { x: 3, y: 4 } in |point|

  point in choice {
      |struct { x, y }| x + y
  }

  // evals to 7
  ```


## Motivation

### Bring back tuples
We previously replaces tuples with records, which was in theory a good idea.
But I miss tuples, and I feel like we should have them back.

You should be able to write
```curse
(1, 2) in |(a, b)|

// do whatever
```

### Tuples break piecewise closure syntax

If we added tuples, the following code would be syntactically ambiguous:
```curse
(
    |0| false,
    |_| true,
)
```

It could either be a tuple of two closures (one of which is non-exhaustive!), or it could be a piecewise closure.
It would be incredibly inconsistent if this _weren't_ a tuple, so I lean towards changing the piecewise closure syntax.

My initial thoughts were to just use different wrapping disambiguators.

```curse
{{
    |0| false,
    |_| true,
}}
```
But this felt a bit odd, and was borderline becoming confusing to mentally parse.

Other languages just have a keyword for branching (`switch`, `case`, `match`, etc.), so making Curse use a keyword to introduce piecewise closures would make a lot of sense.
Since choice types need to be matched on before using, using the `choice` keyword to start a piecewise function would create a nice symmetry between `choice` types and actually using them:
```curse
choice {
    |0| false,
    |_| true,
}
```
I like this idea _a lot_ because:
1. it doesn't add any keywords
2. having a keyword then a braced block feels very familiar
3. still "Curse"-y since it's a function.

In practice, it would be used in combination with the `in` function a lot:
```curse
5 in |x|
x in choice {
    |0| false,
    |_| true,
}

// evals to true
```

### Syntax is no longer LR(1)

So I tried this out an apparently our syntax is no longer LR(1) because of this possible sequence of tokens:

```curse
fn foo = |x| x choice
```

It doesn't know if `foo` is just the identity function and we're about to start a `choice` type definition, or if `choice` is the start of a piecewise closure and we're applying `x` to the lhs of.

I was always a bit dubious about having no terminators on function definitions, but our above ideas would require it to keep the parser simple.
I'm not opposed to this, so how about blocks?

Then this sequence of tokens:
```curse
fn foo = |x| {
    x choice ...
```
... isn't ambiguous anymore, since we're clearly in an expression because we haven't hit the closing `}` yet.

This would require two things: add block expressions, which are basically identical to parenthesized expressions, and require the top-level functions always wrap the body in a block.

Alternatively we could just not have blocks and use parenthesis instead.
But I think that looks bad.

### Is it a record or a block?

But this brings more syntactic ambiguities:
```curse
5 in |num|
{ num }
```
Is this a record with type `{ num: I32 }`, or a block which just evaluates to type `I32`?

There are several possible solutions to this:
1. make record construction require the fields be named every time, e.g. `{ num: num }`.
This removes the syntax sugar (yay) but will lead to a lot of boilerplate (boo).
2. make record construction require commas after every field, e.g. `{ num, }`.
A little weird because `{}` is both the empty block, which evaluates to an empty record, but also an empty record.
So there's ambiguity on what is is, but not what the type or value is.
3. require the `struct` keyword before constructing records, e.g. `struct { num }`.

I'm leaning towards option 3 for two reasons.
First, it's very clear that a struct is being created.
Second, the verbosity discourages rampant abuse of anonymous structs everywhere. I'm in the school of philosophy that believes named types are better for code health, and since both custom structs and anonymous structs will both require an identifier to construct (`Point { .. }` vs `struct { .. }`), there's little reason to not define a named type when appropriate.

Two questions arise from this.
* Should the type of anonymous structs also require the `struct` keyword, e.g. `struct { num: I32 }`?
I'm leaning towards **no**, but I don't have much justification other than that it's not required for disambiguation.
* Should the `struct` keyword be required when matching on an anonymous struct?
I'm leaning towards **yes**, but only because it would feel more consistent.

