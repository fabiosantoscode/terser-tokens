use std::collections::BTreeMap;
use std::usize::MAX;

use crate::{
    basic_blocks::{
        BasicBlock, BasicBlockExit, BasicBlockInstruction, BreakableId, StructuredFlow,
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
            BasicBlockExit::Jump(mrk)
            | BasicBlockExit::Break(mrk)
            | BasicBlockExit::EndFinally(mrk)
                if *mrk == MAX =>
            {
                Some(mrk)
            }
            _ => None,
        }
    } else {
        None
    }
}

fn ensure_forward_jump_marker(out_blocks: &mut Vec<BasicBlock>) -> usize {
    if forward_jump_marker(out_blocks.last_mut()).is_none() {
        out_blocks.push(BasicBlock {
            instructions: vec![],
            exit: BasicBlockExit::Jump(MAX),
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
    let mut leftover_instructions = vec![];
    for item in inp {
        match item {
            StructuredFlow::BasicBlock(instructions) => leftover_instructions.push(instructions),
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

        let instructions = preceding_bbs
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        let item = match item {
            Some(item) => item,
            None => {
                // Will not stand on its own
                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Jump(MAX),
                });
                to_label.push(out_blocks.len() - 1);
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
                let jump_target = jump_targets.get(&brk).unwrap();

                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Continue(jump_target.0),
                });
                break;
            }
            StructuredFlow::Return(exit_type, var_idx) => {
                out_blocks.push(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::ExitFn(exit_type, var_idx.expect("TODO")),
                });
                break;
            }
            // SETS OF BLOCKS (will recurse 1+ times)
            StructuredFlow::Block(blocks_within) => {
                if instructions.len() > 0 {
                    out_blocks.push(BasicBlock {
                        instructions,
                        exit: BasicBlockExit::Jump(MAX),
                    });
                    to_label.push(out_blocks.len() - 1);
                }
                let (block_labels, _) = fold_blocks(out_blocks, blocks_within, jump_targets);

                to_label.extend(block_labels);
            }
            StructuredFlow::Branch(brk, cond_var, cons, alt) => {
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
                out_blocks[finally].exit = BasicBlockExit::EndFinally(MAX);

                to_label.push(finally);
                to_label.extend(body_labels);
                to_label.extend(catch_labels);
                to_label.extend(finally_labels);
                if let Some((_, _, broken_from)) = jump_targets.remove(&brk) {
                    to_label.extend(broken_from);
                }
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
            exit = jump @3
        }
        @2: {
            $2 = 3
            exit = jump @3
        }
        @3: {
            $3 = 4
            exit = return $3
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
                exit = try @2 catch @4 finally @6 after @7
            }
            @2: {
                $0 = 777
                exit = jump @3
            }
            @3: {
                exit = error ? jump @4 : jump @6
            }
            @4: {
                $1 = either($0, $2, $3)
                $2 = 888
                exit = jump @5
            }
            @5: {
                $3 = either($0, $1, $2)
                exit = finally @6 after @7
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = end finally after @8
            }
            @8: {
                $4 = $3
                exit = return $4
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = try @1 catch @2 finally @3 after @3
        }
        @1: {
            $0 = 777
            exit = error ? jump @2 : jump @3
        }
        @2: {
            $1 = either($0, $2, $3)
            $2 = 888
            $3 = either($0, $1, $2)
            exit = finally @3 after @3
        }
        @3: {
            exit = end finally after @4
        }
        @4: {
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
            exit = loop @1..@1
        }
        @1: {
            $1 = 2
            exit = jump @2
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
            exit = loop @1..@3
        }
        @1: {
            $0 = 123
            exit = cond $0 ? @2..@2 : @3..@3
        }
        @2: {
            exit = jump @4
        }
        @3: {
            exit = jump @4
        }
        @4: {
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
