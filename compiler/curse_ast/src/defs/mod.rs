//! In Rust, structs and enum variants can come in one of three flavors:
//! unit, tuple, or record. Because of Curse's cursed syntax, having structs
//! or choice variants be the unit variant isn't possible due to syntactic
//! ambiguity. Consider the following:
//!
//! ```txt
//! x f SomeVariant(a) b ...
//! ```
//! At first glance, it appears as if `a` is an argument for `SomeVariant`,
//! meaning `b` would be a function in function application:
//! ```txt
//! x f (SomeVariant(a)) b ...
//! ```
//! But what if `SomeVariant` was supposed to be the unit variant? Well,
//! `(a)` is just an parenthesized expression, so `b` could be the RHS
//! of a function application:
//! ```txt
//! (x f SomeVariant) (a) b ...
//! ```
//! Since the role of `a` (and `b`) is ambiguous here, it's syntactically
//! ambiguous and requires changes.
//!
//! That change I (Quinn) propose is to make choice variants and structs come
//! in one of _two_ flavors: newtype and record. That is, they can either hold
//! one unnamed value (can be a tuple) or many named values. This solves the
//! ambiguity problem since it means type constructors, e.g. `SomeVariant`,
//! must be followed either by an expression or a named fields wrapped in braces.
//! For example, the above example is no longer ambiguous- it will parse as:
//! ```txt
//! (x f (SomeVariant(a))) b ...
//! ```
//! On the other hand, we can also use named fields:
//!
//! ```txt
//! x f SomeVariant { name: a } b ...
//! ```
//! Which will parse as:
//! ```txt
//! (x f (SomeVariant { name: a })) b ...
//! ```
//!
//! The general rule of thumb is that if we see a type constructor (i.e. PascalCase ident),
//! then if it's followed by `{ field1: x, ... }`, assume it has named fields, otherwise
//! just absorb the next expression and assume it's a newtype constructor.
//!
//! This syntax has several pros and cons.
//!
//! ## Pros
//!
//! A big upside though is that we don't have trailing `)` anymore, so the Rust complaint
//! about ok-wrapping doesn't really apply. Just say `Ok` at the start of an expression
//! it all gets wrapped without the nasty trailing `)` at the end:
//!
//! ```txt
//! Ok x > 0 then x
//! ```
//!
//! ## Cons
//!
//! A downside that affects the practical case is that choice variants (and structs) that
//! would otherwise be the unit flavor must instead be represented as a newtype over `()`.
//! So the variants of `Option` would be `Some T` and `None ()`.
//!
//! Another downside is that constructing nested types may look a little confusing after
//! looking at Curse syntax for awhile. For example, `Ok Some 5` means `Ok(Some(5))`,
//! but also kind of looks like `Ok` and `5` being applied to `Some`.
use crate::{tok, Type};

mod choice_def;
mod struct_def;

pub use choice_def::*;
pub use struct_def::*;

/// 0 or more `T`s separated by `Sep`, with an optional trailing `Sep`.
#[derive(Clone, Debug)]
pub struct Punct<T, Sep> {
    pub elements: Vec<(T, Sep)>,
    pub trailing: Option<T>,
}

/// A named field, e.g. `height: I32`
///
/// This type is used as part of `FieldKind::Record`.
#[derive(Clone, Debug)]
pub struct NamedField<'ast, 'input> {
    pub name: tok::Ident<'input>,
    pub colon: tok::Colon,
    pub ty: &'ast Type<'ast, 'input>,
}

impl<'ast, 'input> NamedField<'ast, 'input> {
    pub fn from_grammar(
        name: tok::Ident<'input>,
        colon: tok::Colon,
        opt_ty: Option<&'ast Type<'ast, 'input>>,
    ) -> Option<Self> {
        Some(NamedField {
            name,
            colon,
            ty: opt_ty?,
        })
    }
}

/// Fields of a struct or choice variant.
#[derive(Clone, Debug)]
pub enum FieldKind<'ast, 'input> {
    Newtype(&'ast Type<'ast, 'input>),
    /// Named fields, e.g. `{ value: I32, visited: Bool }`
    /// Trailing commas _are_ allowed after the last field, and it can also be empty i.e. `{}`.
    Record {
        lbrace: tok::LBrace,
        fields: Punct<NamedField<'ast, 'input>, tok::Comma>,
        rbrace: tok::RBrace,
    },
}

impl<'ast, 'input> FieldKind<'ast, 'input> {
    pub fn newtype_from_grammar(newtype: Option<&'ast Type<'ast, 'input>>) -> Option<Self> {
        Some(FieldKind::Newtype(newtype?))
    }

    pub fn record_from_grammar(
        lbrace: tok::LBrace,
        elements: Vec<(Option<NamedField<'ast, 'input>>, tok::Comma)>,
        trailing: Option<Option<NamedField<'ast, 'input>>>,
        rbrace: tok::RBrace,
    ) -> Option<Self> {
        let elements = elements
            .into_iter()
            .map(|(opt_field, comma)| opt_field.map(|field| (field, comma)))
            .collect::<Option<_>>()?;

        inside_out(trailing).map(|trailing| FieldKind::Record {
            lbrace,
            fields: Punct { elements, trailing },
            rbrace,
        })
    }
}

/// Transposes an `Option<Option<T>>`
fn inside_out<T>(v: Option<Option<T>>) -> Option<Option<T>> {
    match v {
        Some(Some(v)) => Some(Some(v)),
        Some(None) => None,
        None => Some(None),
    }
}
