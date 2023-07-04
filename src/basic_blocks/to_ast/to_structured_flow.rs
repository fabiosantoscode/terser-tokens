use super::super::basic_block::{BasicBlockExit, ExitType};
use super::super::basic_block_group::BasicBlockGroup;
use super::dominator_tree_for_translation::Graph;
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
    Try(Vec<StructuredFlow>),
    Catch(Vec<StructuredFlow>),
    Finally(Vec<StructuredFlow>),
    TryCatch(
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
    ),
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
    TryCatch,
    LoopHeadedBy(usize),
    BlockFollowedBy(usize),
}

pub fn do_tree(func: &BasicBlockGroup) -> StructuredFlow {
    let dom = func.get_dom_graph();

    let tree = do_tree_inner(&dom, 0, Default::default());

    let flat_tree = tree.into_flat_vec();

    let flat_tree = match flat_tree {
        flat_tree if flat_tree.len() == 1 => flat_tree[0].clone(),
        flat_tree => StructuredFlow::Block(flat_tree),
    };

    flat_tree
}

fn do_tree_inner(graph: &Graph, node: usize, context: Ctx) -> StructuredFlow {
    let code_for_node = |context: Ctx| {
        let ys = graph
            .direct_subs(node)
            .into_iter()
            .filter(|child| is_merge_node(graph, *child))
            .collect::<Vec<_>>();
        node_within(graph, node, &ys, context)
    };

    if is_loop_header(graph, node) {
        let inner_context = context.pushed(ContainingSyntax::LoopHeadedBy(node));
        StructuredFlow::Loop(code_for_node(inner_context).into_flat_vec())
    } else {
        code_for_node(context)
    }
}

fn node_within(graph: &Graph, node: usize, ys: &[usize], context: Ctx) -> StructuredFlow {
    match ys.split_first() {
        None => {
            match &graph.nodes[node].basic_block.exit {
                BasicBlockExit::SetTryAndCatch(
                    try_block,
                    catch_block,
                    finally_block,
                    after_block,
                ) => {
                    let inner = context.pushed(ContainingSyntax::TryCatch);
                    return StructuredFlow::TryCatch(
                        do_branch(graph, node, *try_block, inner.clone()).into_flat_vec(),
                        do_branch(graph, node, *catch_block, inner.clone()).into_flat_vec(),
                        do_branch(graph, node, *finally_block, inner.clone()).into_flat_vec(),
                        do_branch(graph, node, *after_block, inner.clone()).into_flat_vec(),
                    );
                }
                BasicBlockExit::PopCatch(catch_block, _finally_or_after) => {
                    return StructuredFlow::BasicBlock(node);
                }
                BasicBlockExit::PopFinally(finally_block, _after) => {
                    return StructuredFlow::BasicBlock(node);
                }
                BasicBlockExit::EndFinally(after_block) => {
                    return StructuredFlow::BasicBlock(node);
                }
                _ => { /* handled below */ }
            };

            let mut block_contents: Vec<StructuredFlow> = vec![StructuredFlow::BasicBlock(node)];

            block_contents.extend(
                match &graph.nodes[node].basic_block.exit {
                    BasicBlockExit::Jump(to) | BasicBlockExit::EndFinally(to) => {
                        do_branch(graph, node, *to, context)
                    }

                    BasicBlockExit::Cond(e, t, f) => {
                        let inner = context.pushed(ContainingSyntax::IfThenElse);
                        StructuredFlow::Branch(
                            *e,
                            vec![do_branch(graph, node, *t, inner.clone())],
                            vec![do_branch(graph, node, *f, inner.clone())],
                        )
                    }
                    BasicBlockExit::ExitFn(exit, ret) => {
                        StructuredFlow::Return(exit.clone(), Some(*ret))
                    }
                    BasicBlockExit::SetTryAndCatch(_, _, _, _)
                    | BasicBlockExit::PopCatch(_, _)
                    | BasicBlockExit::PopFinally(_, _) => {
                        unreachable!("handled above")
                    }
                }
                .into_flat_vec()
                .into_iter(),
            );
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

fn do_branch(graph: &Graph, source: usize, target: usize, context: Ctx) -> StructuredFlow {
    let reverse_postorder = graph.reverse_postorder();
    let index_source = reverse_postorder.iter().position(|&x| x == source).unwrap();
    let index_target = reverse_postorder.iter().position(|&x| x == target).unwrap();

    if
    /* is backwards, so this must be a continuation of an enclosing loop */
    index_target > index_source {
        StructuredFlow::Continue(index_target) // continue the loop
    } else if
    /* a forward branch to a merge node exits a block */
    is_merge_node(graph, target) {
        StructuredFlow::Break(index_target) // break the loop
    } else {
        /* plain goto next */
        do_tree_inner(graph, target, context)
    }
}

// A node 𝑋 that has two or more forward inedges is a merge node. Being a merge node doesn’t
// affect how 𝑋 is translated, but it does affect the placement of 𝑋 ’s translation: the translation
// will follow a block form.
fn is_merge_node(graph: &Graph, child: usize) -> bool {
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
fn is_loop_header(graph: &Graph, node: usize) -> bool {
    let indices = graph.reverse_postorder();
    let node_index = indices.iter().position(|&x| x == node).unwrap();
    let g_node = &graph.nodes[node];

    g_node
        .incoming_edges
        .iter()
        .any(|&edge| indices.iter().position(|&x| x == edge).unwrap() < node_index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::testutils::{parse_basic_blocks, test_basic_blocks};

    #[test]
    fn basic_flow() {
        let func = parse_basic_blocks(
            r###"
        @0: {
            $0 = 123
            exit = jump @1
        }
        @1: {
            $1 = 456
            exit = return $1
        }
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), Return]
        )
        "###);
    }

    #[test]
    fn basic_if() {
        let func = parse_basic_blocks(
            r###"
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
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), Branch 0 {
                [BasicBlockRef(1), Break(0)]
                [BasicBlockRef(2), Break(0)]
            }, BasicBlockRef(3), Return]
        )
        "###);
    }

    #[test]
    fn basic_if_2() {
        let func = parse_basic_blocks(
            r###"
            @0: {
                exit = jump @1
            }
            @1: {
                $0 = 123
                exit = cond $0 ? jump @2 : jump @3
            }
            @2: {
                $1 = 456
                exit = jump @4
            }
            @3: {
                $2 = 789
                exit = jump @4
            }
            @4: {
                $3 = either($1, $2)
                $4 = undefined
                exit = return $4
            }
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), Branch 0 {
                [BasicBlockRef(2), Break(0)]
                [BasicBlockRef(3), Break(0)]
            }, BasicBlockRef(4), Return]
        )
        "###);
    }

    #[test]
    fn basic_while_minimal() {
        let func = parse_basic_blocks(
            r###"
            @0: {
                $0 = 123
                exit = cond $0 ? jump @1 : jump @2
            }
            @1: {
                $1 = 456
                exit = jump @0
            }
            @2: {
                $2 = 789
                exit = return $2
            }
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Loop(
            [BasicBlockRef(0), Branch 0 {
                [BasicBlockRef(1), Continue(2)]
                [BasicBlockRef(2), Return]
            }]
        )
        "###);
    }

    #[test]
    fn basic_while() {
        let func = parse_basic_blocks(
            r###"
            @0: {
                $0 = 123
                exit = cond $0 ? jump @1 : jump @2
            }
            @1: {
                $1 = 456
                exit = jump @0
            }
            @2: {
                $2 = 789
                exit = return $2
            }
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Loop(
            [BasicBlockRef(0), Branch 0 {
                [BasicBlockRef(1), Continue(2)]
                [BasicBlockRef(2), Return]
            }]
        )
        "###);
    }

    #[test]
    fn basic_while_2() {
        let func = parse_basic_blocks(
            r###"
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
            $2 = 999
            $3 = undefined
            exit = return $3
        }
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Loop(
            [BasicBlockRef(0), BasicBlockRef(1), Branch 0 {
                [BasicBlockRef(2), BasicBlockRef(3), Branch 1 {
                    [BasicBlockRef(4), Break(0)]
                    [BasicBlockRef(5), BasicBlockRef(6), Continue(7)]
                }]
                [Break(0)]
            }, BasicBlockRef(7), Return]
        )
        "###);
    }

    #[test]
    fn basic_while_3() {
        let func = parse_basic_blocks(
            r###"
        @0: {
            $0 = 777
            exit = jump @1
        }
        @1: {
            $0 = 777
            exit = jump @2
        }
        @2: {
            exit = cond $0 ? jump @3 : jump @8
        }
        @3: {
            $1 = 888
            exit = jump @4
        }
        @4: {
            exit = cond $1 ? jump @5 : jump @6
        }
        @5: {
            exit = jump @8
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = jump @1
        }
        @8: {
            $2 = 999
            $3 = undefined
            exit = return $3
        }
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), Loop(
                [BasicBlockRef(1), BasicBlockRef(2), Branch 0 {
                    [BasicBlockRef(3), BasicBlockRef(4), Branch 1 {
                        [BasicBlockRef(5), Break(0)]
                        [BasicBlockRef(6), BasicBlockRef(7), Continue(7)]
                    }]
                    [Break(0)]
                }, BasicBlockRef(8), Return]
            )]
        )
        "###);
    }

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
            [BasicBlockRef(0), BasicBlockRef(1), Branch 4 {
                [BasicBlockRef(2), Break(0)]
                [BasicBlockRef(3), Break(0)]
            }, BasicBlockRef(4), Return]
        )
        "###);
    }

    #[test]
    fn an_array() {
        let func = test_basic_blocks("var x = [1, 2 ? 3 : 4, , ...3];");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 1
            $1 = 2
            exit = cond $1 ? jump @1 : jump @2
        }
        @1: {
            $2 = 3
            exit = jump @3
        }
        @2: {
            $3 = 4
            exit = jump @3
        }
        @3: {
            $4 = either($2, $3)
            $5 = 3
            $6 = [$0, $4, , ...$5,]
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
            [BasicBlockRef(0), Branch 1 {
                [BasicBlockRef(1), Break(0)]
                [BasicBlockRef(2), Break(0)]
            }, BasicBlockRef(3), Return]
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
            [BasicBlockRef(0), Branch 0 {
                [BasicBlockRef(1), Branch 1 {
                    [BasicBlockRef(2), Break(3)]
                    [BasicBlockRef(3), Break(3)]
                }, BasicBlockRef(4), BasicBlockRef(5), Break(0)]
                [BasicBlockRef(6), Break(0)]
            }, BasicBlockRef(7), Return]
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
            [BasicBlockRef(0), BasicBlockRef(1), Branch 0 {
                [BasicBlockRef(2), BasicBlockRef(3), Break(3)]
                [Break(3)]
            }, BasicBlockRef(4), BasicBlockRef(5), Branch 3 {
                [BasicBlockRef(6), Break(0)]
                [Break(0)]
            }, BasicBlockRef(7), Return]
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
            999
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
            $2 = 999
            $3 = undefined
            exit = return $3
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Loop(
            [BasicBlockRef(0), BasicBlockRef(1), Branch 0 {
                [BasicBlockRef(2), BasicBlockRef(3), Branch 1 {
                    [BasicBlockRef(4), Break(0)]
                    [BasicBlockRef(5), BasicBlockRef(6), Continue(7)]
                }]
                [Break(0)]
            }, BasicBlockRef(7), Return]
        )
        "###);
    }

    #[test]
    fn mk_trycatch() {
        let func = test_basic_blocks(
            "try {
                777;
            } catch (e) {
                888;
            }
            999",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            exit = jump @1
        }
        @1: {
            exit = try @2 catch @4 finally @6 after @8
        }
        @2: {
            $0 = 777
            exit = jump @3
        }
        @3: {
            exit = error ? jump @4 : jump @6
        }
        @4: {
            $1 = caught_error()
            $2 = 888
            exit = jump @5
        }
        @5: {
            exit = finally @6 after @7
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = end finally after @8
        }
        @8: {
            $3 = 999
            $4 = undefined
            exit = return $4
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), TryCatch(
                [BasicBlockRef(2), BasicBlockRef(3)]
                [BasicBlockRef(4), BasicBlockRef(5)]
                [BasicBlockRef(6), BasicBlockRef(7)]
                [BasicBlockRef(8), Return]
            )]
        )
        "###);
    }

    #[test]
    fn mk_trycatch_2() {
        let func = test_basic_blocks(
            "try {
                777;
            } catch (e) {
                888;
            } finally {
                999;
            }
            111",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            exit = jump @1
        }
        @1: {
            exit = try @2 catch @4 finally @6 after @8
        }
        @2: {
            $0 = 777
            exit = jump @3
        }
        @3: {
            exit = error ? jump @4 : jump @6
        }
        @4: {
            $1 = caught_error()
            $2 = 888
            exit = jump @5
        }
        @5: {
            exit = finally @6 after @7
        }
        @6: {
            $3 = 999
            exit = jump @7
        }
        @7: {
            exit = end finally after @8
        }
        @8: {
            $4 = 111
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
            [BasicBlockRef(0), TryCatch(
                [BasicBlockRef(2), BasicBlockRef(3)]
                [BasicBlockRef(4), BasicBlockRef(5)]
                [BasicBlockRef(6), BasicBlockRef(7)]
                [BasicBlockRef(8), Return]
            )]
        )
        "###);
    }

    #[test]
    fn mk_trycatch_edgecases() {
        let func = test_basic_blocks(
            "try {
                if (111) {222}
            } catch {
            }
            111",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            exit = jump @1
        }
        @1: {
            exit = try @2 catch @8 finally @10 after @12
        }
        @2: {
            $0 = 111
            exit = jump @3
        }
        @3: {
            exit = cond $0 ? jump @4 : jump @6
        }
        @4: {
            $1 = 222
            exit = jump @5
        }
        @5: {
            exit = jump @6
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = error ? jump @8 : jump @10
        }
        @8: {
            exit = jump @9
        }
        @9: {
            exit = finally @10 after @11
        }
        @10: {
            exit = jump @11
        }
        @11: {
            exit = end finally after @12
        }
        @12: {
            $2 = 111
            $3 = undefined
            exit = return $3
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), TryCatch(
                [BasicBlockRef(2), BasicBlockRef(3), Branch 0 {
                    [BasicBlockRef(4), BasicBlockRef(5), Break(6)]
                    [Break(6)]
                }, BasicBlockRef(6), BasicBlockRef(7)]
                [BasicBlockRef(8), BasicBlockRef(9)]
                [BasicBlockRef(10), BasicBlockRef(11)]
                [BasicBlockRef(12), Return]
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
            StructuredFlow::Try(_) => "Try".to_string(),
            StructuredFlow::Catch(_) => "Catch".to_string(),
            StructuredFlow::Finally(_) => "Finally".to_string(),
            StructuredFlow::TryCatch(_, _, _, _) => "TryCatch".to_string(),
        }
    }
    fn into_flat_vec(self) -> Vec<StructuredFlow> {
        let spread_out = |items: Vec<StructuredFlow>| {
            items
                .into_iter()
                .flat_map(StructuredFlow::into_flat_vec)
                .collect::<Vec<_>>()
        };

        match self {
            StructuredFlow::Block(items) => spread_out(items),
            StructuredFlow::Branch(cond, cons, alt) => {
                vec![StructuredFlow::Branch(
                    cond,
                    spread_out(cons),
                    spread_out(alt),
                )]
            }
            StructuredFlow::TryCatch(try_, catch, finally, after) => {
                vec![StructuredFlow::TryCatch(
                    spread_out(try_),
                    spread_out(catch),
                    spread_out(finally),
                    spread_out(after),
                )]
            }
            StructuredFlow::Loop(items) => vec![StructuredFlow::Loop(spread_out(items))],
            StructuredFlow::Try(items) => vec![StructuredFlow::Try(spread_out(items))],
            StructuredFlow::Catch(items) => vec![StructuredFlow::Catch(spread_out(items))],
            StructuredFlow::Finally(items) => {
                vec![StructuredFlow::Finally(spread_out(items))]
            }
            other => vec![other],
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
            StructuredFlow::Try(x) => vec![x.clone()],
            StructuredFlow::Catch(x) => vec![x.clone()],
            StructuredFlow::Finally(x) => vec![x.clone()],
            StructuredFlow::TryCatch(t, v, els, after) => {
                vec![t.clone(), v.clone(), els.clone(), after.clone()]
            }
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
            StructuredFlow::Try(_) => None,
            StructuredFlow::Catch(_) => None,
            StructuredFlow::Finally(_) => None,
            StructuredFlow::TryCatch(_, _, _, _) => None,
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

        if let StructuredFlow::Branch(var, cons, alt) = self {
            let cons = format!("{:?}", cons);
            let alt = format!("{:?}", alt);

            return write!(
                f,
                "{} {} {{\n{}\n{}\n}}",
                self.str_head(),
                var,
                indent_str_lines(&cons),
                indent_str_lines(&alt)
            );
        } else if let Some(index) = self.index() {
            write!(f, "{}({})", self.str_head(), index)
        } else {
            let children = self.children();
            if children.len() == 0 {
                write!(f, "{}", self.str_head())
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
