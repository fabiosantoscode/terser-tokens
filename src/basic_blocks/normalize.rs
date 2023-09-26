use std::collections::{BTreeMap, HashMap, HashSet};

use crate::basic_blocks::{BasicBlock, BasicBlockExit};

pub fn normalize_basic_blocks(blocks: BTreeMap<usize, BasicBlock>) -> BTreeMap<usize, BasicBlock> {
    // return (exits, basic_blocks);

    let jumped_to = get_blocks_jumped_to(&blocks);

    // keep track of eliminated blocks and how labels change around
    let mut eliminated_count = 0;
    let mut swapped_labels: HashMap<usize, usize> = Default::default();

    let reachable_blocks = get_reachable_blocks(&blocks);

    let mut out_blocks: BTreeMap<usize, BasicBlock> = BTreeMap::new();

    for (i, block) in blocks.into_iter() {
        let prev_block = out_blocks.last_key_value().map(|(_, block)| block);

        if !reachable_blocks.contains(&i) {
            eliminated_count += 1;
        } else if unmergeable(&block.exit) {
            out_blocks.insert(i, block);
        } else if should_merge_blocks(&jumped_to, prev_block, &block, i) {
            let mut prev_block = out_blocks.last_entry().unwrap();
            let prev_block = prev_block.get_mut();

            prev_block.exit = block.exit;
            prev_block.instructions.extend(block.instructions);

            eliminated_count += 1;
        } else {
            out_blocks.insert(i, block);
        }

        swapped_labels.insert(i, i - eliminated_count);
    }

    if eliminated_count == 0 {
        out_blocks
    } else {
        out_blocks
            .into_values()
            .enumerate()
            .map(|(i, mut block)| {
                for block_label in block.exit.block_labels_mut() {
                    *block_label = swapped_labels[block_label];
                }

                (i, block)
            })
            .collect()
    }
}

fn should_merge_blocks(
    jumped_to: &HashSet<usize>,
    prev: Option<&BasicBlock>,
    block: &BasicBlock,
    block_idx: usize,
) -> bool {
    match prev {
        Some(BasicBlock {
            exit: prev_exit @ BasicBlockExit::Jump(jump_target),
            instructions: prev_instructions,
        }) =>
        // the blocks are the same -- can merge
        {
            (&block.instructions, &block.exit) == (prev_instructions, prev_exit)
                // the previous block ends with a jump to this block -- can merge unless it's a jump target
                    || (*jump_target == block_idx
                        && !jumped_to.contains(&jump_target)
                        && !jumped_to.contains(&block_idx))
        }
        _ => false,
    }
}

fn unmergeable(exit: &BasicBlockExit) -> bool {
    match exit {
        BasicBlockExit::SetTryAndCatch(_, _, _, _) => true,
        BasicBlockExit::PopCatch(_, _) => true,
        BasicBlockExit::PopFinally(_, _) => true,
        BasicBlockExit::EndFinally(_) => true,
        _ => false,
    }
}

fn get_blocks_jumped_to(blocks: &BTreeMap<usize, BasicBlock>) -> HashSet<usize> {
    blocks
        .iter()
        .map(|(i, block)| (*i, &block.exit))
        .zip(
            blocks
                .iter()
                .map(|(next_i, _)| Some(*next_i))
                .skip(1)
                .chain(vec![None].into_iter()),
        )
        .flat_map(|((_i, e), next_i)| match e {
            BasicBlockExit::Jump(j) => {
                if next_i == Some(*j) {
                    vec![]
                } else {
                    vec![*j]
                }
            }
            BasicBlockExit::Break(j) => vec![*j],
            BasicBlockExit::Continue(j) => vec![*j],
            BasicBlockExit::Cond(_, cons, _, alt, _) => vec![*cons, *alt],
            BasicBlockExit::Loop(start, _end) => vec![*start],
            BasicBlockExit::SetTryAndCatch(try_block, catch_block, finally_block, after) => {
                vec![*try_block, *catch_block, *finally_block, *after]
            }
            BasicBlockExit::PopCatch(catch_block, finally_or_after) => {
                vec![*catch_block, *finally_or_after]
            }
            BasicBlockExit::PopFinally(finally_block, _after_finally) => vec![*finally_block],
            BasicBlockExit::EndFinally(after) => vec![*after],
            BasicBlockExit::ExitFn(_, _) => vec![],
        })
        .collect::<HashSet<_>>()
}

fn get_reachable_blocks(exits: &BTreeMap<usize, BasicBlock>) -> HashSet<usize> {
    let mut reachable_blocks = HashSet::new();
    let mut stack = vec![0];

    use BasicBlockExit::*;

    // Unconditionally reachable blocks, because try..catch is finnicky
    stack.extend(exits.iter().flat_map(|(_, block)| match &block.exit {
        SetTryAndCatch(try_, catch, finally, after) => vec![*try_, *catch, *finally, *after],
        PopCatch(catch, finally) => vec![*catch, *finally],
        PopFinally(finally, _) => vec![*finally],
        EndFinally(after) => vec![*after],
        _ => Vec::with_capacity(0),
    }));

    while let Some(block) = stack.pop() {
        if reachable_blocks.contains(&block) {
            continue;
        }

        reachable_blocks.insert(block);

        stack.extend(exits[&block].exit.jump_targets());
    }

    reachable_blocks
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::BasicBlockGroup;
    use crate::testutils::*;

    fn test_normalize(instructions: &str) -> BasicBlockGroup {
        let group = parse_instructions(instructions);
        let blocks = normalize_basic_blocks(group.blocks);

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
