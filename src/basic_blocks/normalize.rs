use std::collections::HashMap;
use std::collections::HashSet;

use domtree::dfs::DFSGraph;

use crate::basic_blocks::basic_block::BasicBlock;
use crate::basic_blocks::basic_block::{BasicBlockExit, BasicBlockInstruction};
use crate::basic_blocks::basic_block_group::BasicBlockGroup;
use crate::basic_blocks::dominator_tree;

pub fn normalize_basic_blocks(
    exits: &Vec<BasicBlockExit>,
    basic_blocks: &Vec<Vec<(usize, BasicBlockInstruction)>>,
) -> (
    Vec<BasicBlockExit>,
    Vec<Vec<(usize, BasicBlockInstruction)>>,
) {
    let jumped_to = get_blocks_jumped_to(exits);

    // keep track of eliminated block and how labels change around
    let mut eliminated_count = 0;
    let mut swapped_labels: HashMap<usize, usize> = Default::default();

    let out_exits: Vec<BasicBlockExit> = vec![];
    let out_basic_blocks: Vec<Vec<(usize, BasicBlockInstruction)>> = vec![];

    let reachable_blocks = get_reachable_blocks(exits, basic_blocks);

    let (mut out_exits, out_basic_blocks) = exits.iter().zip(basic_blocks.iter()).enumerate().fold(
        (out_exits, out_basic_blocks),
        |(mut out_exits, mut out_basic_blocks), (i, (exit, block))| {
            if !reachable_blocks.contains(&i) {
                eliminated_count += 1;
                return (out_exits, out_basic_blocks);
            }

            let do_merge = match (out_exits.last(), out_basic_blocks.last()) {
                prevvy if prevvy == (Some(exit), Some(block)) => true,
                (Some(BasicBlockExit::Jump(j)), _) if *j == i => {
                    !jumped_to.contains(j)
                        && !jumped_to.contains(&(*j + 1))
                        && !jumped_to.contains(&i)
                }
                _ => false,
            };

            match (do_merge, out_exits.last_mut(), out_basic_blocks.last_mut()) {
                (true, Some(prev_exit), Some(prev_block)) => {
                    *prev_exit = exit.clone();
                    prev_block.extend(block.clone());

                    eliminated_count += 1;
                }
                _ => {
                    out_exits.push(exit.clone());
                    out_basic_blocks.push(block.clone());
                }
            };

            swapped_labels.insert(i, i - eliminated_count);
            (out_exits, out_basic_blocks)
        },
    );

    // adjust labels for however many blocks were eliminated
    for exit in out_exits.iter_mut() {
        match exit {
            BasicBlockExit::Jump(j) => {
                *j = *swapped_labels.get(j).unwrap();
            }
            BasicBlockExit::Cond(_, c, a) => {
                *c = *swapped_labels.get(c).unwrap();
                *a = *swapped_labels.get(a).unwrap();
            }
            BasicBlockExit::ExitFn(_, _) => { /* nothing */ }
        }
    }

    assert_eq!(out_exits.len(), out_basic_blocks.len());

    (out_exits, out_basic_blocks)
}

fn get_blocks_jumped_to(exits: &Vec<BasicBlockExit>) -> HashSet<usize> {
    exits
        .iter()
        .enumerate()
        .flat_map(|(i, e)| match e {
            BasicBlockExit::Jump(j) if *j != i + 1 => vec![*j],
            BasicBlockExit::Cond(_, cons, alt) => vec![*cons, *alt],
            _ => vec![],
        })
        .collect::<HashSet<_>>()
}

fn get_reachable_blocks(exits: &Vec<BasicBlockExit>, basic_blocks: &Vec<Vec<(usize, BasicBlockInstruction)>>) -> HashSet<usize> {
    let func = BasicBlockGroup::from_asts(
        basic_blocks.iter().cloned().zip(exits.iter().cloned())
        .map(
            |(block, exit)| {
                BasicBlock::new(block.clone(), exit.clone())
            },
        ).collect()
    );
    let g = func.get_dom_graph();

    HashSet::<usize>::from_iter(g.post_order_sequence(0).into_iter())
}
