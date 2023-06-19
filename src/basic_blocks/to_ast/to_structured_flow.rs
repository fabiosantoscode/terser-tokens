use super::super::basic_block::{BasicBlockExit, ExitType};
use super::super::basic_block_group::BasicBlockGroup;
use super::super::dominator_tree::Graph;
use std::fmt::{Debug, Formatter};

// https://dl.acm.org/doi/pdf/10.1145/3547621
// This paper really unlocked this project.
// It explains how one can turn basic blocks into a structured AST.
// It's focused on WASM, but I've heard JS also has "if", "break" and "loops"
// So it's probably helpful here too!
//
// What follows is a translation of this paper into Rust.

#[derive(Clone)]
pub enum StructuredFlow {
    Block(Vec<StructuredFlow>),
    Loop(Vec<StructuredFlow>),
    Branch(usize, Vec<StructuredFlow>, Vec<StructuredFlow>),
    Break(usize),
    Continue(usize),
    Return(ExitType, Option<usize>),
    BasicBlock(usize),
    VarRef(usize),
}

// type Context = [ContainingSyntax] -- innermost form first
// data ContainingSyntax
// = IfThenElse
// | LoopHeadedBy Label -- label marks loop header
// | BlockFollowedBy Label -- label marks code right after the block
#[derive(Debug, Default, Clone)]
struct Ctx {
    containing_syntax: Vec<ContainingSyntax>,
}

impl Ctx {
    pub fn pushed(&self, syn: ContainingSyntax) -> Self {
        let mut new = self.clone();
        new.containing_syntax.push(syn);
        new
    }
}

#[derive(Debug, Clone)]
enum ContainingSyntax {
    IfThenElse,
    LoopHeadedBy(usize),
    BlockFollowedBy(usize),
}

pub fn do_tree(func: &BasicBlockGroup) -> StructuredFlow {
    let dom = func.get_dom_graph();

    do_tree_inner(&dom, 0, Default::default())
}

fn do_tree_inner(graph: &Graph, node: usize, context: Ctx) -> StructuredFlow {
    let code_for_node = |context: Ctx| {
        let ys = graph
        .direct_subs(node)
        .into_iter()
        .filter(|child| is_merge_node(graph, *child, 0))
        .collect::<Vec<_>>();
        node_within(
            graph,
            node,
            &ys,
            context,
        )
    };

    if is_loop_header(graph, node, context.clone()) {
        let inner_context = context.pushed(ContainingSyntax::LoopHeadedBy(node));
        StructuredFlow::Loop(vec![code_for_node(inner_context)])
    } else {
        code_for_node(context)
    }
}

fn node_within(graph: &Graph, node: usize, ys: &[usize], context: Ctx) -> StructuredFlow {
    match ys.split_first() {
        None => {
            let mut block_contents: Vec<StructuredFlow> = tx_block(graph, node, context.clone());
            block_contents.push(match &graph.nodes[node].basic_block.exit {
                BasicBlockExit::Jump(to) => do_branch(graph, node, *to, context),
                BasicBlockExit::Cond(e, t, f) => {
                    let inner_context = context.pushed(ContainingSyntax::IfThenElse);

                    StructuredFlow::Branch(
                        *e,
                        vec![do_branch(graph, node, *t, inner_context.clone())],
                        vec![do_branch(graph, node, *f, inner_context.clone())],
                    )
                }
                BasicBlockExit::ExitFn(exit, ret) => {
                    StructuredFlow::Return(exit.clone(), Some(*ret))
                }
            });
            StructuredFlow::Block(block_contents)
        }
        Some((y, ys)) => {
            let inner = vec![
                node_within(
                    graph,
                    node,
                    ys,
                    context.pushed(ContainingSyntax::BlockFollowedBy(*y)),
                ),
                do_tree_inner(&graph, *y, context),
            ];
            StructuredFlow::Block(inner)
        }
    }
}

fn tx_block(graph: &Graph, node: usize, context: Ctx) -> Vec<StructuredFlow> {
    vec![StructuredFlow::BasicBlock(node)]
}

fn do_branch(graph: &Graph, source: usize, target: usize, context: Ctx) -> StructuredFlow {
    let reverse_postorder = graph.reverse_postorder();
    let index_source = reverse_postorder.iter().position(|&x| x == source).unwrap();
    let index_target = reverse_postorder.iter().position(|&x| x == target).unwrap();

    if
    /* is backwards */
    index_target > index_source {
        StructuredFlow::Continue(index_target) // continue the loop
    } else if
    /* multiple nodes come here */
    graph.nodes[target].incoming_edges.len() > 1 {
        StructuredFlow::Break(index_target) // continue the loop
    } else {
        do_tree_inner(graph, target, context)
    }
}

// A node 𝑋 that has two or more forward inedges is a merge node. Being a merge node doesn’t
// affect how 𝑋 is translated, but it does affect the placement of 𝑋 ’s translation: the translation
// will follow a block form.
fn is_merge_node(graph: &Graph, child: usize, root: usize) -> bool {
    let reverse_postorder = graph.reverse_postorder();
    let index = |child| reverse_postorder.iter().position(|&x| x == child).unwrap();
    let forward_inedges = graph.nodes[child]
        .incoming_edges
        .iter()
        .filter(|&&edge| index(edge) > index(child))
        .count();

    forward_inedges >= 2
}

// A node 𝑋 that has a back inedge is a loop header, and its translation is wrapped in a loop
// form. The translation of the subtree rooted at 𝑋 is placed into the body of the loop.
fn is_loop_header(graph: &Graph, node: usize, context: Ctx) -> bool {
    let indices = graph.reverse_postorder();
    let node_index = indices.iter().position(|&x| x == node).unwrap();
    let g_node = &graph.nodes[node];

    g_node.incoming_edges.iter().any(|&edge| {
        if let Some(index_edge) = indices.iter().position(|&x| x == edge) {
            node_index > index_edge
        } else {
            // TODO why would that be None?
            false
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::testutils::test_basic_blocks;

    #[test]
    fn mk_stats() {
        let func = test_basic_blocks("123; 123; 123; 123; 123 ? 456 : 789");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 123
            $1 = 123
            $2 = 123
            $3 = 123
            exit = jump @1
        }
        @1: {
            $4 = 123
            exit = cond $4 ? jump @2 : jump @3
        }
        @2: {
            $5 = 456
            exit = jump @4
        }
        @3: {
            $6 = 789
            exit = jump @4
        }
        @4: {
            $7 = either($5, $6)
            $8 = undefined
            exit = return $8
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), Block(
                [Block(
                    [BasicBlockRef(1), Branch(
                        [Block([BasicBlockRef(2), Break(0)])]
                        [Block([BasicBlockRef(3), Break(0)])]
                    )]
                ), Block([BasicBlockRef(4), Return])]
            )]
        )
        "###);
    }

    #[test]
    fn mk_stats_2() {
        let func = test_basic_blocks("123 ? (456 ? 7 : 8) : 9");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 123
            exit = cond $0 ? jump @1 : jump @6
        }
        @1: {
            $1 = 456
            exit = cond $1 ? jump @2 : jump @3
        }
        @2: {
            $2 = 7
            exit = jump @4
        }
        @3: {
            $3 = 8
            exit = jump @4
        }
        @4: {
            exit = jump @5
        }
        @5: {
            $4 = either($2, $3)
            exit = jump @7
        }
        @6: {
            $5 = 9
            exit = jump @7
        }
        @7: {
            $6 = either($4, $5)
            $7 = undefined
            exit = return $7
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [Block(
                [BasicBlockRef(0), Branch(
                    [Block(
                        [Block(
                            [BasicBlockRef(1), Branch(
                                [Block([BasicBlockRef(2), Break(3)])]
                                [Block([BasicBlockRef(3), Break(3)])]
                            )]
                        ), Block([BasicBlockRef(4), Block([BasicBlockRef(5), Break(0)])])]
                    )]
                    [Block([BasicBlockRef(6), Break(0)])]
                )]
            ), Block([BasicBlockRef(7), Return])]
        )
        "###);
    }

    #[test]
    fn mk_stats_3() {
        let func = test_basic_blocks("if (123) { 345 } 10; if (1) 2");

        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 123
            exit = jump @1
        }
        @1: {
            exit = cond $0 ? jump @2 : jump @4
        }
        @2: {
            $1 = 345
            exit = jump @3
        }
        @3: {
            exit = jump @4
        }
        @4: {
            $2 = 10
            $3 = 1
            exit = jump @5
        }
        @5: {
            exit = cond $3 ? jump @6 : jump @7
        }
        @6: {
            $4 = 2
            exit = jump @7
        }
        @7: {
            $5 = undefined
            exit = return $5
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), Block(
                [Block(
                    [BasicBlockRef(1), Branch(
                        [Block([BasicBlockRef(2), Block([BasicBlockRef(3), Break(3)])])]
                        [Break(3)]
                    )]
                ), Block(
                    [BasicBlockRef(4), Block(
                        [Block(
                            [BasicBlockRef(5), Branch(
                                [Block([BasicBlockRef(6), Break(0)])]
                                [Break(0)]
                            )]
                        ), Block([BasicBlockRef(7), Return])]
                    )]
                )]
            )]
        )
        "###);
    }

    #[test]
    fn mk_loop() {
        let func = test_basic_blocks(
            "
            while (777) {
                if (888) {
                    break;
                }
            }
        ",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 777
            exit = jump @1
        }
        @1: {
            exit = cond $0 ? jump @2 : jump @7
        }
        @2: {
            $1 = 888
            exit = jump @3
        }
        @3: {
            exit = cond $1 ? jump @4 : jump @5
        }
        @4: {
            exit = jump @7
        }
        @5: {
            exit = jump @6
        }
        @6: {
            exit = jump @0
        }
        @7: {
            $2 = undefined
            exit = return $2
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Loop(
            [Block(
                [BasicBlockRef(0), Block(
                    [Block(
                        [BasicBlockRef(1), Branch(
                            [Block(
                                [BasicBlockRef(2), Block(
                                    [BasicBlockRef(3), Branch(
                                        [Block([BasicBlockRef(4), Break(0)])]
                                        [Block([BasicBlockRef(5), Block([BasicBlockRef(6), Continue(7)])])]
                                    )]
                                )]
                            )]
                            [Break(0)]
                        )]
                    ), Block([BasicBlockRef(7), Return])]
                )]
            )]
        )
        "###);
    }
}

// For printing out these trees

impl StructuredFlow {
    fn str_head(&self) -> String {
        match self {
            StructuredFlow::Branch(_, _, _) => "Branch".to_string(),
            StructuredFlow::Break(_) => "Break".to_string(),
            StructuredFlow::Continue(_) => "Continue".to_string(),
            StructuredFlow::Loop(_) => "Loop".to_string(),
            StructuredFlow::Block(_) => "Block".to_string(),
            StructuredFlow::Return(_, _) => "Return".to_string(),
            StructuredFlow::BasicBlock(_) => "BasicBlockRef".to_string(),
            StructuredFlow::VarRef(_) => "VarRef".to_string(),
        }
    }
    fn children(&self) -> Vec<Vec<StructuredFlow>> {
        match self {
            StructuredFlow::Branch(_x /* who cares */, y, z) => vec![y.clone(), z.clone()],
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(x) => vec![x.clone()],
            StructuredFlow::Block(x) => vec![x.clone()],
            StructuredFlow::Return(_, _) => vec![],
            StructuredFlow::BasicBlock(_) => vec![],
            StructuredFlow::VarRef(_) => vec![],
        }
    }
    fn index(&self) -> Option<usize> {
        match self {
            StructuredFlow::Branch(_, _, _) => None,
            StructuredFlow::Break(x) => Some(*x),
            StructuredFlow::Continue(x) => Some(*x),
            StructuredFlow::Loop(_) => None,
            StructuredFlow::Block(_) => None,
            StructuredFlow::Return(_, _) => None,
            StructuredFlow::BasicBlock(x) => Some(*x),
            StructuredFlow::VarRef(x) => Some(*x),
        }
    }
}

impl Debug for StructuredFlow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent_str_lines = |s: &str| {
            let lines = s.lines();
            let indented_lines = lines.map(|line| format!("    {}", line));
            indented_lines.collect::<Vec<String>>().join("\n")
        };

        if let Some(index) = self.index() {
            write!(f, "{}({})", self.str_head(), index)
        } else {
            let children = self.children();
            if children.len() == 0 {
                write!(f, "{}", self.str_head())
            } else if children.len() == 1 {
                let children = format!("{:?}", children[0]);
                let is_multiline = children.contains("\n");
                if is_multiline {
                    let lines = indent_str_lines(&children);
                    return write!(f, "{}(\n{}\n)", self.str_head(), lines);
                }
                write!(f, "{}({})", self.str_head(), children)
            } else {
                let lines = children
                    .iter()
                    .map(|child| indent_str_lines(&format!("{:?}", child)))
                    .collect::<Vec<String>>()
                    .join("\n");

                write!(f, "{}(\n{}\n)", self.str_head(), lines)
            }
        }
    }
}
