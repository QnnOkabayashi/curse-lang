use curse_interner::InternedString;
use crate::cpsexpr::Ident;

pub struct Gensym {
    id: usize,
}

impl Gensym {
    pub fn new() -> Self {
        Gensym {
            id: 0
        }
    }

    pub fn gensym(&mut self, sym: &str) -> Ident {
        self.id += 1;
        InternedString::get_or_intern(&format!("{}__{}_", sym, self.id))
    }
}
