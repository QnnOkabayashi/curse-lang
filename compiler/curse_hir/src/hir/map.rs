use bumpalo_thin_slice::ThinSlice;
use curse_interner::Ident;

#[derive(Copy, Clone, Debug)]
pub enum Binding<'hir> {
    Ident(Ident),
    Record(ThinSlice<'hir, (Self, Option<Self>)>),
    Error,
}
