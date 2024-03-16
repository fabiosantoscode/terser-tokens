use std::collections::BTreeMap;
use std::usize::MAX;

use crate::{
    basic_blocks::{
        BasicBlock, BasicBlockExit, BasicBlockInstruction, BreakableId, StructuredClassMember,
        StructuredFlow,
    },
    block_ops::block_group_to_structured_flow,
};

pub fn normalize_basic_blocks(blocks: BTreeMap<usize, BasicBlock>) -> BTreeMap<usize, BasicBlock> {
    let recursive = block_group_to_structured_flow(blocks);

    normalize_basic_blocks_tree(vec![recursive])
}

pub fn normalize_basic_blocks_tree(recursive: Vec<StructuredFlow>) -> BTreeMap<usize, BasicBlock> {
    let mut out_blocks = vec![];
    let mut jump_targets = BTreeMap::new();
    fold_blocks(&mut out_blocks, recursive, &mut jump_targets);

    out_blocks.into_iter().enumerate().collect()
}

fn forward_jump_marker(block: Option<&mut BasicBlock>) -> Option<&mut usize> {
    if let Some(block) = block {
        match &mut block.exit {
            BasicBlockExit::Break(mrk) if *mrk == MAX => Some(mrk),
            _ => None,
        }
    } else {
        None
    }
}

fn ensure_forward_jump_marker(out_blocks: &mut Vec<BasicBlock>) -> usize {
    let has_forward_jump_marker = matches!(
        out_blocks.last(),
        Some(BasicBlock {
            exit: BasicBlockExit::Fallthrough,
            ..
        })
    ) || forward_jump_marker(out_blocks.last_mut()).is_some();

    if !has_forward_jump_marker {
        out_blocks.push(BasicBlock {
            instructions: vec![],
            exit: BasicBlockExit::Fallthrough,
        });
    }

    out_blocks.len() - 1
}

fn resolve_forward_jumps(out_blocks: &mut Vec<BasicBlock>, to_label: &mut Vec<usize>) {
    let jump_target = out_blocks.len();
    for label in to_label.drain(..) {
        let m = out_blocks.get_mut(label);
        if let Some(marker) = forward_jump_marker(m) {
            *marker = jump_target;
        }
    }
}

fn fold_basic_blocks(
    inp: Vec<StructuredFlow>,
) -> Vec<(
    Vec<Vec<(usize, BasicBlockInstruction)>>,
    Option<StructuredFlow>,
)> {
    let mut out = vec![];
    let mut leftover_instructions = Vec::with_capacity(inp.len());
    for item in inp {
        match item {
            StructuredFlow::BasicBlock(instructions) => leftover_instructions.push(instructions),
            item if item.is_structured_flow_empty() => {}
            _ => {
                if leftover_instructions.len() > 0 {
                    out.push((leftover_instructions, Some(item)));
                    leftover_instructions = vec![];
                } else {
                    out.push((vec![], Some(item)));
                }
            }
        }
    }
    if leftover_instructions.len() > 0 {
        out.push((leftover_instructions, None));
    }
    out
}

fn fold_blocks(
    out_blocks: &mut Vec<BasicBlock>,
    as_tree: Vec<StructuredFlow>,
    jump_targets: &mut BTreeMap<BreakableId, (usize, usize, Vec<usize>)>,
) -> (Vec<usize>, usize) {
    let mut to_label = vec![];

    for (preceding_bbs, item) in fold_basic_blocks(as_tree) {
        resolve_forward_jumps(out_blocks, &mut to_label);

        let instructions = preceding_bbs.into_iter().flatten().collect::<Vec<_>>();

        let item = match item {
            Some(item) => item,
            None => {
                // Will not stand on its own
                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Fallthrough,
                });
                continue;
            }
        };

        match item {
            // THE HUMBLE BASIC BLOCK
            StructuredFlow::BasicBlock(_) => {
                unreachable!("handled above")
            }
            // EXITS
            StructuredFlow::Break(brk) => {
                let (_, _, jump_target) = jump_targets.get_mut(&brk).unwrap();

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Break(MAX),
                });
                jump_target.push(out_blocks.len() - 1);
                break;
            }
            StructuredFlow::Continue(brk) => {
                let (continue_to, _, _) = jump_targets.get(&brk).unwrap();

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Continue(*continue_to),
                });
                break;
            }
            StructuredFlow::Return(exit_type, var_idx) => {
                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::ExitFn(exit_type, var_idx),
                });
                break;
            }
            // SETS OF BLOCKS (will recurse 1+ times)
            StructuredFlow::Block(blocks_within) => {
                if instructions.len() > 0 {
                    out_blocks.push(BasicBlock {
                        instructions,
                        exit: BasicBlockExit::Fallthrough,
                    });
                }
                let (block_labels, _) = fold_blocks(out_blocks, blocks_within, jump_targets);

                to_label.extend(block_labels);
            }
            StructuredFlow::Cond(brk, cond_var, cons, alt) => {
                if brk.0.is_some() {
                    jump_targets.insert(brk, (out_blocks.len() + 1, MAX, vec![]));
                }

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Cond(cond_var, MAX, MAX, MAX, MAX),
                });
                let cond = out_blocks.len() - 1;

                let (cons_labels, cons) = fold_blocks(out_blocks, cons, jump_targets);
                let (alt_labels, alt) = fold_blocks(out_blocks, alt, jump_targets);

                out_blocks[cond].exit =
                    BasicBlockExit::Cond(cond_var, cond + 1, cons, cons + 1, alt);

                to_label.extend(cons_labels);
                to_label.extend(alt_labels);
                if let Some((_, _, broken_from)) = jump_targets.remove(&brk) {
                    to_label.extend(broken_from);
                }
            }
            StructuredFlow::Switch(brk, expression, cases) => {
                if brk.0.is_some() {
                    jump_targets.insert(brk, (out_blocks.len() + 1, MAX, vec![]));
                }

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Switch(expression, MAX, MAX),
                });
                let switch_start = out_blocks.len() - 1;

                for case in cases {
                    match case.condition {
                        Some((insx, condvar)) => {
                            out_blocks.push(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::SwitchCaseExpression(MAX, MAX),
                            });
                            let case_expr_start = out_blocks.len() - 1;
                            let (case_expr_labels, case_expr_end) =
                                fold_blocks(out_blocks, insx, jump_targets);

                            out_blocks.push(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::SwitchCase(Some(condvar), MAX, MAX),
                            });
                            let case_start = out_blocks.len() - 1;

                            let (case_labels, case_end) =
                                fold_blocks(out_blocks, case.body, jump_targets);

                            out_blocks[case_expr_start].exit = BasicBlockExit::SwitchCaseExpression(
                                case_expr_start + 1,
                                case_expr_end,
                            );
                            out_blocks[case_start].exit =
                                BasicBlockExit::SwitchCase(Some(condvar), case_start + 1, case_end);

                            to_label.extend(case_expr_labels);
                            to_label.extend(case_labels);
                        }
                        None => {
                            out_blocks.push(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::SwitchCase(None, MAX, MAX),
                            });
                            let case_start = out_blocks.len() - 1;

                            let (case_labels, case_end) =
                                fold_blocks(out_blocks, case.body, jump_targets);

                            out_blocks[case_start].exit =
                                BasicBlockExit::SwitchCase(None, case_start + 1, case_end);

                            to_label.extend(case_labels);
                        }
                    }
                }

                out_blocks.push(BasicBlock {
                    instructions: vec![],
                    exit: BasicBlockExit::SwitchEnd,
                });
                let switch_end = out_blocks.len() - 1;

                out_blocks[switch_start].exit =
                    BasicBlockExit::Switch(expression, switch_start + 1, switch_end);

                if let Some((_, _, broken_from)) = jump_targets.remove(&brk) {
                    to_label.extend(broken_from);
                }
            }
            StructuredFlow::Loop(brk, body) => {
                if brk.0.is_some() {
                    jump_targets.insert(brk, (out_blocks.len() + 1, MAX, vec![]));
                }

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Loop(MAX, MAX),
                });
                let head = out_blocks.len() - 1;

                let (body_labels, body) = fold_blocks(out_blocks, body, jump_targets);

                out_blocks[head].exit = BasicBlockExit::Loop(head + 1, body);

                to_label.extend(body_labels);
                if let Some((_, _, broken_from)) = jump_targets.remove(&brk) {
                    to_label.extend(broken_from);
                }
            }
            StructuredFlow::ForInOfLoop(brk, looped_var, loop_kind, body) => {
                if brk.0.is_some() {
                    jump_targets.insert(brk, (out_blocks.len() + 1, MAX, vec![]));
                }

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::ForInOfLoop(looped_var, loop_kind, MAX, MAX),
                });
                let head = out_blocks.len() - 1;

                let (body_labels, body) = fold_blocks(out_blocks, body, jump_targets);

                out_blocks[head].exit =
                    BasicBlockExit::ForInOfLoop(looped_var, loop_kind, head + 1, body);

                to_label.extend(body_labels);
                if let Some((_, _, broken_from)) = jump_targets.remove(&brk) {
                    to_label.extend(broken_from);
                }
            }
            StructuredFlow::TryCatch(brk, body, catch, finally) => {
                if brk.0.is_some() {
                    jump_targets.insert(brk, (out_blocks.len() + 1, MAX, vec![]));
                }

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::SetTryAndCatch(MAX, MAX, MAX, MAX),
                });
                let head = out_blocks.len() - 1;

                let (body_labels, _) = fold_blocks(out_blocks, body, jump_targets);
                let body = ensure_forward_jump_marker(out_blocks);

                let (catch_labels, _) = fold_blocks(out_blocks, catch, jump_targets);
                let catch = ensure_forward_jump_marker(out_blocks);

                let (finally_labels, _) = fold_blocks(out_blocks, finally, jump_targets);
                let finally = ensure_forward_jump_marker(out_blocks);

                out_blocks[head].exit =
                    BasicBlockExit::SetTryAndCatch(head + 1, body + 1, catch + 1, finally);
                out_blocks[body].exit = BasicBlockExit::PopCatch(body + 1, catch + 1);
                out_blocks[catch].exit = BasicBlockExit::PopFinally(catch + 1, finally);

                to_label.extend(body_labels);
                to_label.extend(catch_labels);
                to_label.extend(finally_labels);
                if let Some((_, _, broken_from)) = jump_targets.remove(&brk) {
                    to_label.extend(broken_from);
                }
            }
            StructuredFlow::Class(class_var, members) => {
                // TODO push/pop jump_targets? It's invalid to `break` out of a static block
                let head = out_blocks.len();

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::ClassStart(class_var, head + 1, MAX),
                });

                for member in members.into_iter() {
                    match member {
                        StructuredClassMember::Property(block, prop) => {
                            let mut to_label = vec![];

                            let (body_labels, _) = fold_blocks(out_blocks, block, jump_targets);
                            to_label.extend(body_labels);

                            out_blocks.push(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::ClassProperty(prop.clone()),
                            });
                        }
                        StructuredClassMember::Constructor(func_id) => {
                            out_blocks.push(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::ClassConstructor(func_id),
                            });
                        }
                        StructuredClassMember::StaticBlock(block) => {
                            out_blocks.push(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::ClassStaticBlock(MAX, MAX),
                            });

                            let sb_start = out_blocks.len() - 1;

                            let (body_labels, sb_end) =
                                fold_blocks(out_blocks, block, jump_targets);

                            to_label.extend(body_labels);

                            out_blocks[sb_start].exit =
                                BasicBlockExit::ClassStaticBlock(sb_start + 1, sb_end + 1);
                        }
                    }
                }

                out_blocks.push(BasicBlock {
                    instructions: vec![],
                    exit: BasicBlockExit::ClassEnd,
                });
                let end = out_blocks.len() - 1;

                out_blocks[head].exit = BasicBlockExit::ClassStart(class_var, head + 1, end);
            }
            StructuredFlow::Debugger => {
                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Debugger,
                });
            }
        }
    }

    (to_label, out_blocks.len() - 1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::BasicBlockGroup;
    use crate::testutils::*;

    fn test_normalize(instructions: &str) -> BasicBlockGroup {
        let group = parse_instructions(instructions);
        let blocks = normalize_basic_blocks(group.blocks);

        let blocks_again = normalize_basic_blocks(blocks.clone());
        assert_eq!(
            blocks, blocks_again,
            "normalizing twice should be idempotent"
        );

        BasicBlockGroup {
            blocks,
            ..Default::default()
        }
    }

    #[test]
    fn test_normalize_if() {
        let group = test_normalize(
            "@0: {
                $0 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                $1 = 2
                exit = jump @3
            }
            @2: {
                $2 = 3
                exit = jump @3
            }
            @3: {
                $3 = 4
                exit = return $3
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            $0 = 1
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $1 = 2
        }
        @2: {
            $2 = 3
        }
        @3: {
            $3 = 4
            exit = return $3
        }
        "###);
    }

    #[test]
    fn test_normalize_if_empty() {
        let group = test_normalize(
            "@0: {
                $0 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                exit = jump @3
            }
            @2: {
                exit = jump @3
            }
            @3: {
                $1 = 4
                exit = return $1
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            $0 = 1
            $1 = 4
            exit = return $1
        }
        "###);
    }

    #[test]
    fn test_normalize_trycatch() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = try @2 catch @4 finally @6..@7
            }
            @2: {
                $0 = 777
                exit = jump @3
            }
            @3: {
                exit = catch @4..@6
            }
            @4: {
                $1 = either($0, $2, $3)
                $2 = 888
                exit = jump @5
            }
            @5: {
                $3 = either($0, $1, $2)
                exit = finally @6..@7
            }
            @6: {
                exit = jump @7
            }
            @7: {
            }
            @8: {
                $4 = $3
                exit = return $4
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = try @1 catch @2 finally @3..@3
        }
        @1: {
            $0 = 777
            exit = catch @2..@3
        }
        @2: {
            $1 = either($0, $2, $3)
            $2 = 888
            $3 = either($0, $1, $2)
            exit = finally @3..@3
        }
        @3: {
        }
        @4: {
            $4 = $3
            exit = return $4
        }
        "###);
    }

    #[test]
    fn test_normalize_trycatch_empty() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = try @2 catch @4 finally @6..@7
            }
            @2: {
                exit = jump @3
            }
            @3: {
                exit = catch @4..@6
            }
            @4: {
                $1 = either($2, $3)
                $2 = 888
                exit = jump @5
            }
            @5: {
                $3 = either($1, $2)
                exit = finally @6..@7
            }
            @6: {
                exit = jump @7
            }
            @7: {
            }
            @8: {
                $4 = $3
                exit = return $4
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            $4 = $3
            exit = return $4
        }
        "###);
    }

    #[test]
    fn test_normalize_loop() {
        let group = test_normalize(
            "@0: {
                exit = loop @1..@2
            }
            @1: {
                $1 = 2
                exit = break @3
            }
            @2: {
                $2 = 3
                exit = jump @3
            }
            @3: {
                $3 = 4
                exit = return $3
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = loop @1..@1
        }
        @1: {
            $1 = 2
            exit = break @2
        }
        @2: {
            $3 = 4
            exit = return $3
        }
        "###);
    }

    #[test]
    fn test_normalize_loop_break() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = loop @2..@9
            }
            @2: {
                $0 = 123
                exit = cond $0 ? @3..@8 : @9..@9
            }
            @3: {
                exit = jump @4
            }
            @4: {
                exit = jump @11
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
                exit = jump @2
            }
            @9: {
                exit = jump @10
            }
            @10: {
                exit = jump @11
            }
            @11: {
                exit = jump @12
            }
            @12: {
                $1 = undefined
                exit = return $1
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = loop @1..@1
        }
        @1: {
            $0 = 123
        }
        @2: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }

    #[test]
    fn test_normalize_loop_break_2() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = loop @2..@8
            }
            @2: {
                $0 = 123
                exit = cond $0 ? @3..@7 : @8..@8
            }
            @3: {
                exit = jump @4
            }
            @4: {
                exit = break @9
            }
            @5: {
                exit = jump @6
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = continue @2
            }
            @8: {
                exit = break @9
            }
            @9: {
                exit = jump @10
            }
            @10: {
                $1 = undefined
                exit = return $1
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = loop @1..@3
        }
        @1: {
            $0 = 123
            exit = cond $0 ? @2..@2 : @3..@3
        }
        @2: {
            exit = break @4
        }
        @3: {
            exit = break @4
        }
        @4: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }
}
