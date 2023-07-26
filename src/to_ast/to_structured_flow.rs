use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use deep_bind::contextual;

use crate::basic_blocks::{BasicBlockExit, BasicBlockGroup, ExitType};

use super::dominator_tree_for_translation::Graph;

// https://dl.acm.org/doi/pdf/10.1145/3547621
// This paper really unlocked this project.
// It explains how one can turn basic blocks into a structured AST.
// It's focused on WASM, but I've heard JS also has "if", "break" and "loops"
// So it's probably helpful here too!
//
// What follows is a translation of this paper into Rust.

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct BreakableId(Option<usize>);

impl std::fmt::Display for BreakableId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(brk) = self.0 {
            write!(f, "#{}", brk)?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub enum StructuredFlow {
    Block(BreakableId, Vec<StructuredFlow>),
    Loop(BreakableId, Vec<StructuredFlow>),
    Branch(BreakableId, usize, Vec<StructuredFlow>, Vec<StructuredFlow>),
    TryCatch(
        BreakableId,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
    ),
    Break(BreakableId),
    Continue(BreakableId),
    Return(ExitType, Option<usize>),
    BasicBlock(usize),
}

// type Context = [ContainingSyntax] -- innermost form first
// data ContainingSyntax
// = IfThenElse
// | LoopHeadedBy Label -- label marks loop header
// | BlockFollowedBy Label -- label marks code right after the block
#[derive(Debug, Clone)]
struct Ctx {
    pub graph: Rc<Graph>,
    pub containing_syntax: RefCell<Vec<(BreakableId, ContainingSyntax)>>,
    pub positions_in_reverse_postorder: Vec<usize>,
}

impl Ctx {
    /// Create a context from the graph, and cache reverse postorder lookups
    pub fn from_graph(graph: Graph) -> Self {
        let reverse_postorder = graph.reverse_postorder();
        let mut positions_in_reverse_postorder = reverse_postorder.clone();
        for (i, node) in reverse_postorder.iter().enumerate() {
            positions_in_reverse_postorder[*node] = i;
        }

        Ctx {
            graph: Rc::new(graph),
            containing_syntax: Default::default(),
            positions_in_reverse_postorder,
        }
    }

    pub fn push_within<T, Fnc>(&self, syn: (BreakableId, ContainingSyntax), func: Fnc) -> T
    where
        Fnc: FnOnce() -> T,
    {
        self.containing_syntax.borrow_mut().push(syn);
        let ret = func();
        self.containing_syntax.borrow_mut().pop();
        ret
    }

    pub fn node_index_in_reverse_postorder(&self, node_id: usize) -> usize {
        self.positions_in_reverse_postorder[node_id]
    }

    ///
    pub fn containing_syntax_index(&self, target_id: usize, is_brk: bool) -> BreakableId {
        let containing_syntax = self.containing_syntax.borrow();

        let index = containing_syntax
            .iter()
            .enumerate()
            .rev()
            .find_map(|(index, (_id, item))| match item {
                ContainingSyntax::LoopHeadedBy(x) | ContainingSyntax::BlockFollowedBy(x) => {
                    if x == &target_id {
                        Some(index)
                    } else {
                        None
                    }
                }
                ContainingSyntax::IfThenElse => None, // TODO what do?
                _ => todo!("break indices? {item:?}"),
            })
            .expect("break/continue without matching to a container");

        let index = if is_brk { index + 1 } else { index };

        containing_syntax[index].0
    }

    /// According to our current context, where will we fall through to?
    /// do_branch calls this so it can omit spurious break/continue
    pub fn get_fall_through_target(&self) -> Option<usize> {
        let containing_syntax = self.containing_syntax.borrow();

        containing_syntax
            .iter()
            .rev()
            .find_map(|(_, item)| match item {
                ContainingSyntax::LoopHeadedBy(x) | ContainingSyntax::BlockFollowedBy(x) => {
                    Some(*x)
                }
                _ => None,
            })
    }
}

#[derive(Debug, Clone)]
enum ContainingSyntax {
    IfThenElse,
    TryCatch,
    LoopHeadedBy(usize),
    BlockFollowedBy(usize),
}

contextual!(Context(CONTEXT): Option<Rc<Ctx>> = None);
contextual!(BreakableIdCounter(BREAKABLE_ID): usize = 1);

fn context() -> Rc<Ctx> {
    Context::clone().unwrap()
}

fn get_breakable_id() -> BreakableId {
    BREAKABLE_ID.with(|id| {
        *id.borrow_mut() += 1;
        BreakableId(Some(*id.borrow()))
    })
}

pub fn do_tree(func: &BasicBlockGroup) -> StructuredFlow {
    let dom = func.get_dom_graph();

    BreakableIdCounter::replace_within(1, || {
        Context::replace_within(Some(Rc::new(Ctx::from_graph(dom))), || {
            let tree = do_tree_inner(0);

            tree.simplify()
        })
    })
}

fn do_tree_inner(node: usize) -> StructuredFlow {
    let code_for_node = || {
        let ys = context()
            .graph
            .direct_subs(node)
            .into_iter()
            .filter(|child| is_merge_node(*child))
            .collect::<Vec<_>>();
        node_within(node, &ys)
    };

    if is_loop_header(node) {
        let id = get_breakable_id();

        context().push_within((id, ContainingSyntax::LoopHeadedBy(node)), || {
            StructuredFlow::Loop(id, vec![code_for_node()])
        })
    } else {
        code_for_node()
    }
}

fn node_within(node: usize, ys: &[usize]) -> StructuredFlow {
    match ys.split_first() {
        None => {
            match &context().graph.nodes[node].basic_block.exit {
                BasicBlockExit::SetTryAndCatch(
                    try_block,
                    catch_block,
                    finally_block,
                    after_block,
                ) => {
                    let id = get_breakable_id();
                    return context().push_within((id, ContainingSyntax::TryCatch), || {
                        StructuredFlow::TryCatch(
                            id,
                            do_branch(node, *try_block),
                            do_branch(node, *catch_block),
                            do_branch(node, *finally_block),
                            do_branch(node, *after_block),
                        )
                    });
                }
                BasicBlockExit::PopCatch(_catch_block, _finally_or_after) => {
                    return StructuredFlow::BasicBlock(node);
                }
                BasicBlockExit::PopFinally(_finally_block, _after) => {
                    return StructuredFlow::BasicBlock(node);
                }
                BasicBlockExit::EndFinally(_after_block) => {
                    return StructuredFlow::BasicBlock(node);
                }
                _ => { /* handled below */ }
            };

            let unused_id = get_breakable_id(); // TODO make this dummy, make it crash?
            StructuredFlow::Block(
                unused_id,
                vec![
                    vec![StructuredFlow::BasicBlock(node)],
                    (match &context().graph.nodes[node].basic_block.exit {
                        BasicBlockExit::Jump(to) | BasicBlockExit::EndFinally(to) => {
                            do_branch(node, *to)
                        }

                        BasicBlockExit::Cond(e, t, f) => {
                            let id = get_breakable_id();
                            context().push_within((id, ContainingSyntax::IfThenElse), || {
                                vec![StructuredFlow::Branch(
                                    id,
                                    *e,
                                    do_branch(node, *t),
                                    do_branch(node, *f),
                                )]
                            })
                        }
                        BasicBlockExit::ExitFn(exit, ret) => {
                            vec![StructuredFlow::Return(exit.clone(), Some(*ret))]
                        }
                        BasicBlockExit::SetTryAndCatch(_, _, _, _)
                        | BasicBlockExit::PopCatch(_, _)
                        | BasicBlockExit::PopFinally(_, _) => {
                            unreachable!("handled above")
                        }
                    }),
                ]
                .into_iter()
                .flatten()
                .collect(),
            )
        }
        Some((y, ys)) => {
            let wrapping_id = get_breakable_id();
            let id = get_breakable_id();
            StructuredFlow::Block(
                wrapping_id,
                vec![
                    context().push_within((id, ContainingSyntax::BlockFollowedBy(*y)), || {
                        node_within(node, ys)
                    }),
                    do_tree_inner(*y),
                ],
            )
        }
    }
}

fn do_branch(source: usize, target: usize) -> Vec<StructuredFlow> {
    let index_source = context().node_index_in_reverse_postorder(source);
    let index_target = context().node_index_in_reverse_postorder(target);

    let emit_jump_if_needed = |jump: StructuredFlow| {
        // when doBranch is given a target label that immediately follows the
        // hole in its context, it omits the br instruction.
        if context().get_fall_through_target() == Some(target) {
            vec![]
        } else {
            vec![jump]
        }
    };

    if index_target > index_source {
        /* is backwards, so this must be a continuation of an enclosing loop */
        emit_jump_if_needed(StructuredFlow::Continue(
            context().containing_syntax_index(target, false),
        ))
    // continue the loop
    } else if is_merge_node(target) {
        /* a forward branch to a merge node exits a block */
        emit_jump_if_needed(StructuredFlow::Break(
            context().containing_syntax_index(target, true),
        ))
    } else {
        /* plain goto next */
        vec![do_tree_inner(target)]
    }
}

// A node 𝑋 that has two or more forward inedges is a merge node. Being a merge node doesn’t
// affect how 𝑋 is translated, but it does affect the placement of 𝑋 ’s translation: the translation
// will follow a block form.
fn is_merge_node(child: usize) -> bool {
    let forward_inedges = context().graph.nodes[child]
        .incoming_edges
        .iter()
        .filter(|&&edge| {
            context().node_index_in_reverse_postorder(edge)
                > context().node_index_in_reverse_postorder(child)
        })
        .count();

    forward_inedges >= 2
}

// A node 𝑋 that has a back inedge is a loop header, and its translation is wrapped in a loop
// form. The translation of the subtree rooted at 𝑋 is placed into the body of the loop.
fn is_loop_header(node: usize) -> bool {
    let node_index = context().node_index_in_reverse_postorder(node);
    let g_node = &context().graph.nodes[node];

    g_node
        .incoming_edges
        .iter()
        .any(|&edge| context().node_index_in_reverse_postorder(edge) < node_index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn basic_flow() {
        let func = parse_instructions(
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
        Block (
            [BasicBlockRef(0), BasicBlockRef(1), Return]
        )
        "###);
    }

    #[test]
    fn basic_if() {
        let func = parse_instructions(
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
        Block (
            [BasicBlockRef(0), Branch  ($0) {
                [BasicBlockRef(1)]
                [BasicBlockRef(2)]
            }, BasicBlockRef(3), Return]
        )
        "###);
    }

    #[test]
    fn basic_if_2() {
        let func = parse_instructions(
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
        Block (
            [BasicBlockRef(0), BasicBlockRef(1), Branch  ($0) {
                [BasicBlockRef(2)]
                [BasicBlockRef(3)]
            }, BasicBlockRef(4), Return]
        )
        "###);
    }

    #[test]
    fn basic_while_minimal() {
        let func = parse_instructions(
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
        Loop (
            [BasicBlockRef(0), Branch  ($0) {
                [BasicBlockRef(1)]
                [BasicBlockRef(2), Return]
            }]
        )
        "###);
    }

    #[test]
    fn basic_while_minimal_2() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 123
                exit = cond $0 ? jump @1 : jump @3
            }
            @1: {
                $1 = 456
                exit = cond $1 ? jump @3 : jump @2
            }
            @2: {
                $2 = 456
                exit = jump @0
            }
            @3: {
                $3 = 789
                exit = return $2
            }
        "###,
        );

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Loop #2(
            [BasicBlockRef(0), Branch  ($0) {
                [BasicBlockRef(1), Branch  ($1) {
                    []
                    [BasicBlockRef(2), Continue #2]
                }]
                []
            }, BasicBlockRef(3), Return]
        )
        "###);
    }

    #[test]
    fn basic_while_2() {
        let func = parse_instructions(
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
        Loop #2(
            [BasicBlockRef(0), BasicBlockRef(1), Branch  ($0) {
                [BasicBlockRef(2), BasicBlockRef(3), Branch  ($1) {
                    [BasicBlockRef(4)]
                    [BasicBlockRef(5), BasicBlockRef(6), Continue #2]
                }]
                []
            }, BasicBlockRef(7), Return]
        )
        "###);
    }

    #[test]
    fn basic_while_3() {
        let func = parse_instructions(
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
        Block (
            [BasicBlockRef(0), Loop #3(
                [BasicBlockRef(1), BasicBlockRef(2), Branch  ($0) {
                    [BasicBlockRef(3), BasicBlockRef(4), Branch  ($1) {
                        [BasicBlockRef(5)]
                        [BasicBlockRef(6), BasicBlockRef(7), Continue #3]
                    }]
                    []
                }, BasicBlockRef(8), Return]
            )]
        )
        "###);
    }

    #[test]
    fn mk_stats() {
        let func = parse_instructions(
            r###"
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
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), BasicBlockRef(1), Branch  ($4) {
                [BasicBlockRef(2)]
                [BasicBlockRef(3)]
            }, BasicBlockRef(4), Return]
        )
        "###);
    }

    #[test]
    fn an_array() {
        let func = parse_instructions(
            r###"
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
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), Branch  ($1) {
                [BasicBlockRef(1)]
                [BasicBlockRef(2)]
            }, BasicBlockRef(3), Return]
        )
        "###);
    }

    #[test]
    fn mk_stats_2() {
        let func = parse_instructions(
            r###"
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
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), Branch  ($0) {
                [BasicBlockRef(1), Branch  ($1) {
                    [BasicBlockRef(2)]
                    [BasicBlockRef(3)]
                }, BasicBlockRef(4), BasicBlockRef(5)]
                [BasicBlockRef(6)]
            }, BasicBlockRef(7), Return]
        )
        "###);
    }

    #[test]
    fn mk_stats_3() {
        let func = parse_instructions(
            r###"
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
                exit = cond $3 ? jump @6 : jump @8
            }
            @6: {
                $4 = 2
                exit = jump @7
            }
            @7: {
                exit = jump @8
            }
            @8: {
                $5 = undefined
                exit = return $5
            }
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), BasicBlockRef(1), Branch  ($0) {
                [BasicBlockRef(2), BasicBlockRef(3)]
                []
            }, BasicBlockRef(4), BasicBlockRef(5), Branch  ($3) {
                [BasicBlockRef(6), BasicBlockRef(7)]
                []
            }, BasicBlockRef(8), Return]
        )
        "###);
    }

    #[test]
    fn mk_loop() {
        let func = parse_instructions(
            r###"
            @0: {
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
                exit = jump @10
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = jump @1
            }
            @8: {
                exit = jump @9
            }
            @9: {
                exit = jump @10
            }
            @10: {
                $2 = 999
                $3 = undefined
                exit = return $3
            }
            "###,
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
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
            exit = jump @10
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = jump @1
        }
        @8: {
            exit = jump @9
        }
        @9: {
            exit = jump @10
        }
        @10: {
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
        Block (
            [BasicBlockRef(0), Loop #3(
                [BasicBlockRef(1), BasicBlockRef(2), Branch  ($0) {
                    [BasicBlockRef(3), BasicBlockRef(4), Branch  ($1) {
                        [BasicBlockRef(5)]
                        [BasicBlockRef(6), BasicBlockRef(7), Continue #3]
                    }]
                    [BasicBlockRef(8), BasicBlockRef(9)]
                }, BasicBlockRef(10), Return]
            )]
        )
        "###);
    }

    #[test]
    fn mk_loop_break() {
        let func = parse_instructions(
            r###"
            @0: {
                exit = jump @1
            }
            @1: {
                $0 = 777
                exit = jump @2
            }
            @2: {
                exit = cond $0 ? jump @3 : jump @12
            }
            @3: {
                $1 = 888
                exit = jump @4
            }
            @4: {
                exit = cond $1 ? jump @5 : jump @7
            }
            @5: {
                $2 = 123
                exit = jump @6
            }
            @6: {
                exit = jump @7
            }
            @7: {
                $3 = 889
                exit = jump @8
            }
            @8: {
                exit = cond $3 ? jump @9 : jump @10
            }
            @9: {
                exit = jump @14
            }
            @10: {
                exit = jump @11
            }
            @11: {
                exit = jump @1
            }
            @12: {
                exit = jump @13
            }
            @13: {
                exit = jump @14
            }
            @14: {
                $4 = 999
                $5 = undefined
                exit = return $5
            }
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), Loop #3(
                [BasicBlockRef(1), BasicBlockRef(2), Branch  ($0) {
                    [BasicBlockRef(3), BasicBlockRef(4), Branch  ($1) {
                        [BasicBlockRef(5), BasicBlockRef(6)]
                        []
                    }, BasicBlockRef(7), BasicBlockRef(8), Branch  ($3) {
                        [BasicBlockRef(9)]
                        [BasicBlockRef(10), BasicBlockRef(11), Continue #3]
                    }]
                    [BasicBlockRef(12), BasicBlockRef(13)]
                }, BasicBlockRef(14), Return]
            )]
        )
        "###);
    }

    #[test]
    fn mk_trycatch() {
        let func = parse_instructions(
            r###"
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
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), TryCatch #3(
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
        let func = parse_instructions(
            r###"
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
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), TryCatch #3(
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
        let func = parse_instructions(
            r###"
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
            "###,
        );

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", g.reverse_postorder());

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block (
            [BasicBlockRef(0), TryCatch #3(
                [BasicBlockRef(2), BasicBlockRef(3), Branch  ($0) {
                    [BasicBlockRef(4), BasicBlockRef(5)]
                    []
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
            StructuredFlow::Branch(_, _, _, _) => "Branch".to_string(),
            StructuredFlow::Break(_) => "Break".to_string(),
            StructuredFlow::Continue(_) => "Continue".to_string(),
            StructuredFlow::Loop(_, _) => "Loop".to_string(),
            StructuredFlow::Block(_, _) => "Block".to_string(),
            StructuredFlow::Return(_, _) => "Return".to_string(),
            StructuredFlow::BasicBlock(_) => "BasicBlockRef".to_string(),
            StructuredFlow::TryCatch(_, _, _, _, _) => "TryCatch".to_string(),
        }
    }
    fn simplify(self) -> Self {
        let break_targets = self.get_all_break_targets();
        let mut flat = self.flatten(&break_targets);
        flat.remove_unused_break_ids(&break_targets);
        flat
    }
    fn flatten(self, break_targets: &HashSet<BreakableId>) -> StructuredFlow {
        let breaks = |id: &BreakableId| break_targets.contains(id);
        let map = fix_fn::fix_fn!(|map, items: Vec<StructuredFlow>| -> Vec<StructuredFlow> {
            items
                .into_iter()
                .flat_map(|item| match item {
                    StructuredFlow::Block(id, items) => {
                        if !breaks(&id) {
                            map(items)
                        } else {
                            items
                        }
                    }
                    _ => vec![item.flatten(break_targets)],
                })
                .collect::<Vec<_>>()
        });

        match self {
            StructuredFlow::Block(id, items) => {
                let items = map(items);
                if items.len() == 1 && !breaks(&id) {
                    items.into_iter().next().unwrap()
                } else {
                    StructuredFlow::Block(id, items)
                }
            }
            StructuredFlow::Branch(id, cond, cons, alt) => {
                StructuredFlow::Branch(id, cond, map(cons), map(alt))
            }
            StructuredFlow::TryCatch(id, try_, catch, finally, after) => {
                StructuredFlow::TryCatch(id, map(try_), map(catch), map(finally), map(after))
            }
            StructuredFlow::Loop(id, items) => StructuredFlow::Loop(id, map(items)),
            no_children => no_children,
        }
    }
    fn remove_unused_break_ids(&mut self, used_break_targets: &HashSet<BreakableId>) {
        if let Some(id) = self.breakable_id() {
            if !used_break_targets.contains(&id) {
                self.remove_break_id();
            }
        }

        for children in self.children_mut().iter_mut() {
            for child in children.iter_mut() {
                child.remove_unused_break_ids(used_break_targets);
            }
        }
    }

    fn children(&self) -> Vec<&Vec<StructuredFlow>> {
        match self {
            StructuredFlow::Branch(_id, _x /* who cares */, y, z) => vec![y, z],
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, x) => vec![x],
            StructuredFlow::Block(_, x) => vec![x],
            StructuredFlow::Return(_, _) => vec![],
            StructuredFlow::BasicBlock(_) => vec![],
            StructuredFlow::TryCatch(_, t, v, els, after) => {
                vec![t, v, els, after]
            }
        }
    }

    fn children_mut(&mut self) -> Vec<&mut Vec<StructuredFlow>> {
        match self {
            StructuredFlow::Branch(_id, _x /* who cares */, y, z) => vec![y, z],
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, x) => vec![x],
            StructuredFlow::Block(_, x) => vec![x],
            StructuredFlow::Return(_, _) => vec![],
            StructuredFlow::BasicBlock(_) => vec![],
            StructuredFlow::TryCatch(_, t, v, els, after) => {
                vec![t, v, els, after]
            }
        }
    }

    fn get_all_break_targets(&self) -> HashSet<BreakableId> {
        let mut ret = HashSet::new();
        if let Some(breakable_id) = self.breaks_to_id() {
            ret.insert(breakable_id);
        }

        for child in self.children() {
            for child in child {
                for t in child.get_all_break_targets() {
                    ret.insert(t);
                }
            }
        }

        ret
    }

    fn breaks_to_id(&self) -> Option<BreakableId> {
        match self {
            StructuredFlow::Break(id) | StructuredFlow::Continue(id) => Some(*id),
            _ => None,
        }
    }
    fn remove_break_id(&mut self) {
        match self {
            StructuredFlow::Block(id, _)
            | StructuredFlow::Branch(id, _, _, _)
            | StructuredFlow::Loop(id, _) => *id = BreakableId(None),
            _ => {}
        }
    }

    fn breakable_id(&self) -> Option<BreakableId> {
        match self {
            StructuredFlow::Block(id, _)
            | StructuredFlow::Branch(id, _, _, _)
            | StructuredFlow::Break(id)
            | StructuredFlow::Continue(id)
            | StructuredFlow::Loop(id, _)
            | StructuredFlow::TryCatch(id, _, _, _, _) => Some(*id),
            _ => None,
        }
    }
    fn index_for_formatting(&self) -> Option<usize> {
        match self {
            StructuredFlow::Branch(_, _, _, _) => None,
            StructuredFlow::Break(x) | StructuredFlow::Continue(x) => x.0,
            StructuredFlow::Loop(_, _) => None,
            StructuredFlow::Block(_, _) => None,
            StructuredFlow::Return(_, _) => None,
            StructuredFlow::BasicBlock(x) => Some(*x),
            StructuredFlow::TryCatch(_, _, _, _, _) => None,
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

        write!(f, "{}", self.str_head())?;
        if let Some(breakable_idx) = self.breakable_id() {
            write!(f, " {}", breakable_idx)?;
        }

        match self {
            StructuredFlow::Continue(_) | StructuredFlow::Break(_) => return Ok(()),
            _ => {}
        };

        if let StructuredFlow::Branch(_, var, cons, alt) = self {
            let cons = format!("{:?}", cons);
            let alt = format!("{:?}", alt);

            return write!(
                f,
                " (${}) {{\n{}\n{}\n}}",
                var,
                indent_str_lines(&cons),
                indent_str_lines(&alt)
            );
        } else if let Some(index) = self.index_for_formatting() {
            write!(f, "({})", index)
        } else {
            let children = self.children();
            if children.len() > 0 {
                let lines = children
                    .iter()
                    .map(|child| indent_str_lines(&format!("{:?}", child)))
                    .collect::<Vec<String>>()
                    .join("\n");

                write!(f, "(\n{}\n)", lines)?;
            }

            Ok(())
        }
    }
}
