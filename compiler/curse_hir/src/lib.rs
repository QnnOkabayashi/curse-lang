//! The Curse High-level Intermediate Representation (HIR).
//!
//! Differences from AST:
//! 1. All identifiers are interned.
//! 2. Tokens used for disambiguation are removed (e.g. parenthesized exprs, commas, ...).
//! 3. All sequences are allocated as contiguous slices in an arena.
//!
//! Essentially, the HIR is a boiled down version of the AST, in the sense that it cuts away
//! everything that's unnecessary for analysis, but doesn't actually do any analysis yet.

#![forbid(unsafe_code)]

pub mod hir;
