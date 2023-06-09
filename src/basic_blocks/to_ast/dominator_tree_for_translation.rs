use std::{cell::UnsafeCell, collections::HashMap, hash::Hash, iter::Cloned};

use domtree::{
    dfs::DFSGraph,
    frontier::DominanceFrontier,
    set::{AssocSet, MemberSet},
    DomTree,
};

use super::super::{
    basic_block::{BasicBlock, BasicBlockExit},
    basic_block_group::BasicBlockGroup,
};

#[derive(Clone)]
pub struct VecSet<Y>(Vec<Y>);
#[derive(Clone, Debug)]
pub struct HashMemberSet<T>(std::collections::HashSet<T>);

impl<Y: Clone + Default> AssocSet<usize, Y> for VecSet<Y> {
    fn get(&self, target: usize) -> Y {
        self.0[target].clone()
    }

    fn set(&mut self, key: usize, val: Y) {
        self.0[key] = val;
    }
}

impl<T: PartialEq + Eq + Hash + Clone> MemberSet<T> for HashMemberSet<T> {
    fn contains(&self, target: T) -> bool {
        self.0.contains(&target)
    }

    fn insert(&mut self, target: T) {
        self.0.insert(target);
    }

    type MemberIter<'a> = Cloned<std::collections::hash_set::Iter<'a, T>> where Self : 'a;

    fn iter<'a>(&'a self) -> Self::MemberIter<'a> {
        self.0.iter().cloned()
    }
}

#[derive(Clone, Default)]
pub struct DomTreeable {
    nodes: Vec<BasicBlock>,
}

impl From<BasicBlockGroup> for DomTreeable {
    fn from(value: BasicBlockGroup) -> Self {
        DomTreeable {
            nodes: value.iter().map(|(_, node)| node.clone()).collect(),
        }
    }
}

#[derive(Debug)]
pub struct Node {
    pub tag: usize,                                  // node's identifier
    pub dom: Option<usize>,                          // node's immediate dominator
    pub frontiers: UnsafeCell<HashMemberSet<usize>>, // node's dominance frontiers
    pub incoming_edges: Vec<usize>,                  // node's in-edges
    pub outgoing_edges: Vec<usize>,                  // node's out-edges
    pub basic_block: BasicBlock,                     // node's blocks AST
}

#[derive(Debug)]
pub struct Graph {
    pub nodes: Vec<Node>,
}

impl Graph {
    pub fn direct_subs(&self, of: usize) -> Vec<usize> {
        self.nodes
            .iter()
            .filter(|n| n.dom == Some(of))
            .filter(|n| n.tag != of)
            .map(|n| n.tag)
            .collect::<Vec<_>>()
    }
    pub fn reverse_postorder(&self) -> Vec<usize> {
        let mut result = Vec::new();

        let recurse = fix_fn::fix_fn!(|recurse, result: &mut Vec<usize>, node: usize| -> () {
            if result.contains(&node) {
                return;
            }
            result.push(node);

            for child in self.direct_subs(node) {
                recurse(result, child);
            }
        });

        recurse(&mut result, 0);

        result.reverse();

        result
    }
}

impl DFSGraph for Graph {
    type Identifier = usize;
    type Set<Y> = VecSet<Y>  where Y: Clone + Default;
    type SuccessorIter<'a> = Cloned<std::slice::Iter<'a, usize>> where Self: 'a;

    fn create_set<Y>(&self) -> Self::Set<Y>
    where
        Y: Clone + Default,
    {
        let mut data = Vec::new();
        data.resize(self.nodes.len(), Default::default());
        VecSet(data)
    }

    fn outgoing_edges<'a>(&'a self, id: Self::Identifier) -> Self::SuccessorIter<'a> {
        self.nodes[id].outgoing_edges.iter().cloned()
    }
}

impl DomTree for Graph {
    type MutDomIter<'a> = std::iter::Map<std::slice::IterMut<'a, Node>, fn(&'a mut Node)->&'a mut Option<usize>> where Self: 'a;
    type PredecessorIter<'a> = Cloned<std::slice::Iter<'a, usize>> where Self: 'a;

    fn dom(&self, id: Self::Identifier) -> Option<Self::Identifier> {
        self.nodes[id].dom.clone()
    }

    fn set_dom(&mut self, id: Self::Identifier, target: Option<Self::Identifier>) {
        self.nodes[id].dom = target;
    }

    fn predecessor_iter<'a>(&'a self, id: Self::Identifier) -> Self::PredecessorIter<'a> {
        self.nodes[id].incoming_edges.iter().cloned()
    }

    fn doms_mut<'a>(&'a mut self) -> Self::MutDomIter<'a> {
        self.nodes.iter_mut().map(|x| &mut x.dom)
    }
}

impl DominanceFrontier for Graph {
    type FrontierSet = HashMemberSet<usize>;
    type NodeIter<'a> = std::ops::Range<usize> where Self: 'a ;

    fn frontiers_cell(&self, id: Self::Identifier) -> &UnsafeCell<Self::FrontierSet> {
        &self.nodes[id].frontiers
    }

    fn node_iter<'a>(&'a self) -> Self::NodeIter<'a> {
        0..self.nodes.len()
    }
}

fn to_structured_flow_jump_targets(node: &BasicBlock) -> Vec<usize> {
    match node.exit {
        BasicBlockExit::SetTryAndCatch(t, c, f, a) => {
            vec![t, c, f, a]
        }
        BasicBlockExit::PopCatch(c, f) => {
            vec![]
        }
        BasicBlockExit::PopFinally(c, f) => {
            vec![]
        }
        BasicBlockExit::EndFinally(f) => {
            vec![]
        }
        _ => node.jump_targets(),
    }
}

impl BasicBlockGroup {
    pub fn get_dom_graph(&self) -> Graph {
        let mut all_incoming = HashMap::new();
        for (tag, node) in self.iter() {
            for target in to_structured_flow_jump_targets(node) {
                all_incoming
                    .entry(target)
                    .or_insert_with(Vec::new)
                    .push(*tag);
            }
        }

        let mut g = Graph {
            nodes: self
                .iter()
                .map(|(tag, node)| Node {
                    tag: *tag,
                    dom: None,
                    frontiers: UnsafeCell::new(HashMemberSet(std::collections::HashSet::new())),
                    incoming_edges: all_incoming.get(&tag).cloned().unwrap_or_default(),
                    outgoing_edges: to_structured_flow_jump_targets(node),
                    basic_block: node.clone(),
                })
                .collect(),
        };

        g.populate_dom(0);
        g.populate_frontiers();

        g
    }
}

#[test]
fn test_dominance() {
    use crate::basic_blocks::testutils::test_basic_blocks;

    let block = test_basic_blocks(
        "var x = 0;
        while (x < 10) {
            x = x + 1;
        }",
    );

    let g = block.get_dom_graph();

    println!("{:#?}", block);

    for (id, _node) in block.iter() {
        for dom in g.dom_iter(*id) {
            println!("{} doms {:?}", id, dom);
        }
    }

    // nothing tested here since it's all the domtree crate
}
