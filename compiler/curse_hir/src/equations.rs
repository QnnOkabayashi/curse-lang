use crate::Type;
use displaydoc::Display;
use petgraph::{dot::Dot, prelude::UnGraphMap};
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Display, Hash)]
#[displaydoc("{ty}")]
pub struct TypeData<'hir> {
    pub ty: &'hir Type<'hir>,
    // also info about where in source code
    // this type appears.
    // Most of the time, it will probably be
    // the type of an `Appl`
}

#[derive(Display)]
#[displaydoc("")]
pub struct UnifyData {
    // information about where the unification happened
    // store information about the AST (which has information about src code)
}

#[derive(Default)]
pub struct Equations<'hir> {
    graph: UnGraphMap<TypeData<'hir>, UnifyData>,
}

impl<'hir> Equations<'hir> {
    pub fn new() -> Self {
        Equations {
            graph: UnGraphMap::new(),
        }
    }

    pub fn unify(&mut self, t1: &'hir Type<'hir>, t2: &'hir Type<'hir>) {
        self.graph
            .add_edge(TypeData { ty: t1 }, TypeData { ty: t2 }, UnifyData {});
    }

    // fn solve(&mut self, typevars: &mut [Typevar<'hir>]) {
    //     for mut scc in petgraph::algo::kosaraju_scc(&self.graph) {
    //         let base = scc.pop().expect("scc contains at least one index");
    //         let x = self.graph[base].ty;
    //         for component in scc {
    //             let y = self.graph[component].ty;
    //             // unify x and y
    //         }
    //     }
    // }
}

impl fmt::Display for Equations<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&Dot::new(&self.graph), f)
    }
}
