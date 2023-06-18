use super::dominator_tree::Graph;
use super::ssa_ast::{ExitType, SsaAstNode, SsaExit};
use super::ssa_fn::SsaFn;
use domtree::frontier::DominanceFrontier;
use domtree::DomTree;
use std::collections::{HashMap, HashSet};
use swc_ecma_ast::{Expr, Ident, ReturnStmt, Stmt};

impl SsaFn {
    pub fn to_js_ast(&self) -> () {
        todo!()
    }
}

#[derive(Debug, Default, Clone)]
enum StructuredFlow {
    #[default]
    Nop,
    Straight(Vec<StructuredFlow>),
    Branch(
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
    ),
    Break,
    Loop(Vec<StructuredFlow>),
    Block(Vec<StructuredFlow>),
    Return(ExitType),
    SsaBlock(usize),
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

#[derive(Debug, Clone)]
enum ContainingSyntax {
    IfThenElse,
    LoopHeadedBy(usize),
    BlockFollowedBy(usize),
}

// https://dl.acm.org/doi/pdf/10.1145/3547621
//
fn do_tree(func: &SsaFn)  -> StructuredFlow{
    let dom = func.get_dom_graph();

    do_tree_inner(&dom, 0, Default::default())
}

fn do_tree_inner(graph: &Graph, node: usize, context: Ctx) -> StructuredFlow {
    let code_for_x = |context: Ctx| {
        node_within(
            graph,
            node,
            graph
                .direct_subs(node)
                .iter()
                .filter(|child| is_merge_node(graph, **child, 0))
                .collect(),
            context,
        )
    };

    if
    /* node is a loop header (TODO) */
    false {
        let mut inner_context = context.clone();
        inner_context
            .containing_syntax
            .push(ContainingSyntax::LoopHeadedBy(node));
        StructuredFlow::Loop(vec![code_for_x(inner_context)])
    } else {
        code_for_x(context)
    }
}

fn node_within(graph: &Graph, node: usize, ys: Vec<&usize>, context: Ctx) -> StructuredFlow {
    match ys.split_first() {
        None => {
            let mut block_contents: Vec<StructuredFlow> = tx_block(graph, node, context.clone());
            block_contents.push(match &graph.nodes[node].ssa.exit {
                SsaExit::Jump(to) => do_branch(graph, node, *to, context),
                SsaExit::Cond(e, t, f) => {
                    let inner_context = {
                        let mut inner_context = context.clone();
                        inner_context
                            .containing_syntax
                            .push(ContainingSyntax::IfThenElse);
                        inner_context
                    };
                    StructuredFlow::Branch(
                        tx_block(graph, node, context.clone()),
                        vec![do_branch(graph, node, *t, inner_context.clone())],
                        vec![do_branch(graph, node, *f, inner_context.clone())],
                    )
                }
                SsaExit::ExitFn(exit, ret) => StructuredFlow::Return(exit.clone()),
            });
            StructuredFlow::Block(block_contents)
        }
        Some((y, ys)) => {
            let inner = vec![
                node_within(graph, node, ys.iter().copied().collect(), {
                    let mut inner_context = context.clone();
                    inner_context
                        .containing_syntax
                        .push(ContainingSyntax::BlockFollowedBy(**y));
                    inner_context
                }),
                do_tree_inner(&graph, **y, context),
            ];
            StructuredFlow::Block(inner)
        }
    }
}

fn tx_block(graph: &Graph, node: usize, context: Ctx) -> Vec<StructuredFlow> {
    vec![StructuredFlow::SsaBlock(node)]
}

fn tx_expr(node: usize, expr: Expr) -> Vec<StructuredFlow> {
    vec![StructuredFlow::SsaBlock(node)]
}

fn do_branch(graph: &Graph, source: usize, target: usize, context: Ctx) -> StructuredFlow {
    let reverse_postorder = reverse_postorder(graph);
    let index_source = reverse_postorder.iter().position(|&x| x == source).unwrap();
    let index_target = reverse_postorder.iter().position(|&x| x == target).unwrap();

    if
    /* is backwards */
    index_target > index_source {
        todo!("backwards jumps {source} -> {target} ({index_source} -> {index_target}) result in a 'continue'")
    } else if
    /* multiple nodes come here */
    graph.nodes[target].incoming_edges.len() > 1 {
        StructuredFlow::Break // TODO "WasmBr i -- exit"
    } else {
        do_tree_inner(graph, target, context)
    }
}

fn reverse_postorder(graph: &Graph) -> Vec<usize> {
    let mut result = Vec::new();

    let recurse = fix_fn::fix_fn!(|recurse, result: &mut Vec<usize>, node: usize| -> () {
        if result.contains(&node) {
            return;
        }
        result.push(node);

        for child in graph.direct_subs(node) {
            recurse(result, child);
        }
    });

    recurse(&mut result, 0);

    result.reverse();

    result
}

fn is_merge_node(graph: &Graph, child: usize, root: usize) -> bool {
    // TODO - is this a merge node?
    return false;
}

#[cfg(test)]
mod tests {
    use crate::ssa::{
        testutils::test_ssa_block,
        to_ast::{do_tree, reverse_postorder},
    };

    #[test]
    fn mk_stats() {
        let func = test_ssa_block("123 ? 456 : 789");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 123
            exit = cond $0 ? jump @1 : jump @2
        }
        @1: {
            $1 = 456
            exit = jump @3
        }
        @2: {
            $2 = 789
            exit = jump @3
        }
        @3: {
            $3 = either($1, $2)
            $4 = undefined
            exit = return $4
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", reverse_postorder(&g));

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [
                SsaBlock(
                    0,
                ),
                Branch(
                    [
                        SsaBlock(
                            0,
                        ),
                    ],
                    [
                        Block(
                            [
                                SsaBlock(
                                    1,
                                ),
                                Break,
                            ],
                        ),
                    ],
                    [
                        Block(
                            [
                                SsaBlock(
                                    2,
                                ),
                                Break,
                            ],
                        ),
                    ],
                ),
            ],
        )
        "###);
    }

    #[test]
    fn mk_stats_2() {
        let func = test_ssa_block("123 ? (456 ? 7 : 8) : 9");
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
        println!("{:?}", reverse_postorder(&g));

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [
                SsaBlock(
                    0,
                ),
                Branch(
                    [
                        SsaBlock(
                            0,
                        ),
                    ],
                    [
                        Block(
                            [
                                SsaBlock(
                                    1,
                                ),
                                Branch(
                                    [
                                        SsaBlock(
                                            1,
                                        ),
                                    ],
                                    [
                                        Block(
                                            [
                                                SsaBlock(
                                                    2,
                                                ),
                                                Break,
                                            ],
                                        ),
                                    ],
                                    [
                                        Block(
                                            [
                                                SsaBlock(
                                                    3,
                                                ),
                                                Break,
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                    [
                        Block(
                            [
                                SsaBlock(
                                    6,
                                ),
                                Break,
                            ],
                        ),
                    ],
                ),
            ],
        )
        "###);
    }

    #[test]
    fn mk_stats_3() {
        let func = test_ssa_block("
            while (777) {
                if (888) {
                    break;
                }
            }
        ");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 777
            exit = jump @1
        }
        @1: {
            exit = cond $0 ? jump @2 : jump @9
        }
        @2: {
            $1 = 888
            exit = jump @3
        }
        @3: {
            exit = cond $1 ? jump @4 : jump @7
        }
        @4: {
            exit = jump @6
        }
        @5: {
            exit = jump @6
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = jump @8
        }
        @8: {
            exit = jump @0
        }
        @9: {
            $2 = undefined
            exit = return $2
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", reverse_postorder(&g));

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [
                SsaBlock(
                    0,
                ),
                Block(
                    [
                        SsaBlock(
                            1,
                        ),
                        Branch(
                            [
                                SsaBlock(
                                    1,
                                ),
                            ],
                            [
                                Block(
                                    [
                                        SsaBlock(
                                            2,
                                        ),
                                        Block(
                                            [
                                                SsaBlock(
                                                    3,
                                                ),
                                                Branch(
                                                    [
                                                        SsaBlock(
                                                            3,
                                                        ),
                                                    ],
                                                    [
                                                        Block(
                                                            [
                                                                SsaBlock(
                                                                    4,
                                                                ),
                                                                Break,
                                                            ],
                                                        ),
                                                    ],
                                                    [
                                                        Break,
                                                    ],
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                            [
                                Block(
                                    [
                                        SsaBlock(
                                            9,
                                        ),
                                        Return(
                                            Return,
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                ),
            ],
        )
        "###);
    }
}
