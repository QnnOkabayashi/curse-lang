//! Lowers the AST to the HIR.

mod error;
mod lowerer;

pub use error::{LoweringError, UnexpectedTypeArgs};
pub use lowerer::{Lower, Lowerer};

