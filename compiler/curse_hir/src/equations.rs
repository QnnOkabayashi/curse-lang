use crate::{Type, Var};
use displaydoc::Display;
use petgraph::dot::Dot;
use petgraph::graph::{DiGraph, EdgeReference, NodeIndex};
use petgraph::visit::EdgeRef;
use std::fmt;

#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Equiv {
    #[displaydoc("≡")]
    Yes,
    #[displaydoc("≢")]
    No,
}

/// A node on the inference graph.
#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Rule<'hir> {
    #[displaydoc("{0} {2} {1}")]
    Equivalent(&'hir Type<'hir>, &'hir Type<'hir>, Equiv),
    #[displaydoc("{var} := {definition}")]
    Binding {
        var: Var,
        definition: &'hir Type<'hir>,
    },
}

/// An edge on the inference graph i.e. the reason why a proof (node) leads to
/// a conclusion (another node).
#[derive(Display)]
pub enum Reason {
    /// lhs
    FunctionLhs,
    /// rhs
    FunctionRhs,
    /// output
    FunctionOutput,
    /// transitivity
    Transitivity,
    /// tuple_{0}
    Tuple(usize),
}

#[derive(Default)]
pub struct Equations<'hir> {
    pub graph: DiGraph<Rule<'hir>, Reason>,
}

impl<'hir> Equations<'hir> {
    pub fn new() -> Self {
        Equations {
            graph: DiGraph::new(),
        }
    }

    pub fn add_rule(&mut self, rule: Rule<'hir>) -> NodeIndex {
        self.graph.add_node(rule)
    }

    pub fn add_proof(&mut self, proof: NodeIndex, conclusion: NodeIndex, why: Reason) {
        self.graph.add_edge(proof, conclusion, why);
    }
}

impl fmt::Display for Equations<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn get_edge_attributes(
            graph: &DiGraph<Rule<'_>, Reason>,
            edge: EdgeReference<Reason>,
        ) -> String {
            if let Rule::Equivalent(_, _, Equiv::No) = &graph[edge.source()] {
                "color = red".to_string()
            } else {
                String::new()
            }
        }

        fn get_node_attributes(
            _graph: &DiGraph<Rule<'_>, Reason>,
            (_ix, rule): (NodeIndex, &Rule<'_>),
        ) -> String {
            if let Rule::Equivalent(_, _, Equiv::No) = rule {
                "color = red".to_string()
            } else {
                String::new()
            }
        }

        Dot::with_attr_getters(&self.graph, &[], &get_edge_attributes, &get_node_attributes).fmt(f)
    }
}
