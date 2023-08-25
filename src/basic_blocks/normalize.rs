use std::collections::HashMap;
use std::collections::HashSet;

use crate::basic_blocks::{BasicBlockExit, BasicBlockInstruction};

pub fn normalize_basic_blocks(
    exits: Vec<BasicBlockExit>,
    basic_blocks: Vec<Vec<(usize, BasicBlockInstruction)>>,
) -> (
    Vec<BasicBlockExit>,
    Vec<Vec<(usize, BasicBlockInstruction)>>,
) {
    // return (exits, basic_blocks);

    let jumped_to = get_blocks_jumped_to(&exits);

    // keep track of eliminated block and how labels change around
    let mut eliminated_count = 0;
    let mut swapped_labels: HashMap<usize, usize> = Default::default();

    let out_exits: Vec<BasicBlockExit> = vec![];
    let out_basic_blocks: Vec<Vec<(usize, BasicBlockInstruction)>> = vec![];

    let reachable_blocks = get_reachable_blocks(&exits);

    let (mut exits, basic_blocks) = exits
        .into_iter()
        .zip(basic_blocks.into_iter())
        .enumerate()
        .fold(
            (out_exits, out_basic_blocks),
            |(mut out_exits, mut out_basic_blocks), (i, (exit, block))| {
                if just_skip(&exit) {
                    swapped_labels.insert(i, i - eliminated_count);
                    out_exits.push(exit);
                    out_basic_blocks.push(block);

                    return (out_exits, out_basic_blocks);
                }
                if !reachable_blocks.contains(&i) {
                    eliminated_count += 1;
                    swapped_labels.insert(i, i - eliminated_count);
                    return (out_exits, out_basic_blocks);
                }

                let do_merge = match (out_exits.last(), out_basic_blocks.last()) {
                    (Some(prev_exit @ BasicBlockExit::Jump(_)), Some(prev_block))
                        if (prev_exit, prev_block) == (&exit, &block) =>
                    {
                        true
                    }
                    (Some(BasicBlockExit::Jump(j)), _) if *j == i => {
                        !jumped_to.contains(j)
                            && !jumped_to.contains(&(*j + 1))
                            && !jumped_to.contains(&i)
                    }
                    _ => false,
                };

                match (do_merge, out_exits.last_mut(), out_basic_blocks.last_mut()) {
                    (true, Some(prev_exit), Some(prev_block)) => {
                        *prev_exit = exit;
                        prev_block.extend(block);

                        eliminated_count += 1;
                    }
                    _ => {
                        out_exits.push(exit);
                        out_basic_blocks.push(block);
                    }
                };

                swapped_labels.insert(i, i - eliminated_count);
                (out_exits, out_basic_blocks)
            },
        );

    // adjust labels for however many blocks were eliminated
    for exit in exits.iter_mut() {
        *exit = exit.swap_labels(&swapped_labels);
    }

    assert_eq!(exits.len(), basic_blocks.len());

    (exits, basic_blocks)
}

fn just_skip(exit: &BasicBlockExit) -> bool {
    match exit {
        BasicBlockExit::SetTryAndCatch(_, _, _, _) => true,
        BasicBlockExit::PopCatch(_, _) => true,
        BasicBlockExit::PopFinally(_, _) => true,
        BasicBlockExit::EndFinally(_) => true,
        _ => false,
    }
}

fn get_blocks_jumped_to(exits: &Vec<BasicBlockExit>) -> HashSet<usize> {
    exits
        .iter()
        .enumerate()
        .flat_map(|(i, e)| match e {
            BasicBlockExit::Jump(j) => {
                if *j != i + 1 {
                    vec![*j]
                } else {
                    vec![]
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

fn get_reachable_blocks(exits: &Vec<BasicBlockExit>) -> HashSet<usize> {
    let mut reachable_blocks = HashSet::new();
    let mut stack = vec![0];

    while let Some(block) = stack.pop() {
        if reachable_blocks.contains(&block) {
            continue;
        }

        reachable_blocks.insert(block);

        stack.extend(exits[block].jump_targets());
    }

    reachable_blocks
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::{BasicBlock, BasicBlockGroup};
    use crate::testutils::*;

    fn test_normalize(instructions: &str) -> BasicBlockGroup {
        let group = parse_instructions(instructions);
        let blocks = group.blocks;

        let block_exits = blocks
            .iter()
            .map(|block| block.exit.clone())
            .collect::<Vec<_>>();
        let block_instructions = blocks
            .into_iter()
            .map(|block| block.instructions)
            .collect::<Vec<_>>();

        let (block_exits, block_instructions) =
            normalize_basic_blocks(block_exits, block_instructions);

        BasicBlockGroup::from_asts(
            block_instructions
                .into_iter()
                .zip(block_exits.into_iter())
                .map(|(block, exit)| BasicBlock::new(block, exit))
                .collect(),
        )
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
            exit = jump @1
        }
        @1: {
            exit = loop @2..@4
        }
        @2: {
            $0 = 123
            exit = cond $0 ? @3..@3 : @4..@4
        }
        @3: {
            exit = jump @6
        }
        @4: {
            exit = jump @5
        }
        @5: {
            exit = jump @6
        }
        @6: {
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
            exit = jump @1
        }
        @1: {
            exit = loop @2..@4
        }
        @2: {
            $0 = 123
            exit = cond $0 ? @3..@3 : @4..@4
        }
        @3: {
            exit = break @5
        }
        @4: {
            exit = break @5
        }
        @5: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }
}
