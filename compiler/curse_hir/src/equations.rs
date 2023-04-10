use crate::{Type, Var};
use displaydoc::Display;
use petgraph::dot::Dot;
use petgraph::graph::{DiGraph, EdgeReference, NodeIndex};
use petgraph::visit::EdgeRef;
use std::fmt;

/// A node on the inference graph.
#[derive(Copy, Clone, Debug, Display, PartialEq)]
pub enum Node<'hir> {
    #[displaydoc("{0} ≡ {1}")]
    Equiv(&'hir Type<'hir>, &'hir Type<'hir>),
    #[displaydoc("{0} ≢ {1}")]
    NotEquiv(&'hir Type<'hir>, &'hir Type<'hir>),
    #[displaydoc("{var} := {definition}")]
    Binding {
        var: Var,
        definition: &'hir Type<'hir>,
    },
}

/// An edge on the inference graph i.e. the reason why a proof (node) leads to
/// a conclusion (another node).
#[derive(Display)]
pub enum Edge {
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
    pub graph: DiGraph<Node<'hir>, Edge>,
}

impl<'hir> Equations<'hir> {
    pub fn new() -> Self {
        Equations {
            graph: DiGraph::new(),
        }
    }

    pub fn add_rule(&mut self, rule: Node<'hir>) -> NodeIndex {
        self.graph.add_node(rule)
    }

    pub fn add_proof(&mut self, proof: NodeIndex, conclusion: NodeIndex, edge: Edge) {
        self.graph.add_edge(proof, conclusion, edge);
    }
}

impl fmt::Display for Equations<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn get_edge_attributes(
            graph: &DiGraph<Node<'_>, Edge>,
            edge: EdgeReference<Edge>,
        ) -> String {
            if let Node::NotEquiv(_, _) = &graph[edge.source()] {
                "color = red".to_string()
            } else {
                String::new()
            }
        }

        fn get_node_attributes(
            _graph: &DiGraph<Node<'_>, Edge>,
            (_ix, rule): (NodeIndex, &Node<'_>),
        ) -> String {
            if let Node::NotEquiv(_, _) = rule {
                "color = red".to_string()
            } else {
                String::new()
            }
        }

        Dot::with_attr_getters(&self.graph, &[], &get_edge_attributes, &get_node_attributes).fmt(f)
    }
}
