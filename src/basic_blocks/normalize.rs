use std::collections::HashMap;
use std::collections::HashSet;

use crate::basic_blocks::basic_block::{BasicBlockExit, BasicBlockInstruction};

pub fn normalize_basic_blocks(
    exits: &Vec<BasicBlockExit>,
    basic_blocks: &Vec<Vec<(usize, BasicBlockInstruction)>>,
) -> (
    Vec<BasicBlockExit>,
    Vec<Vec<(usize, BasicBlockInstruction)>>,
) {
    let jumped_to = exits
        .iter()
        .enumerate()
        .flat_map(|(i, e)| match e {
            BasicBlockExit::Jump(j) if *j != i + 1 => vec![*j],
            BasicBlockExit::Cond(_, cons, alt) => vec![*cons, *alt],
            _ => vec![],
        })
        .collect::<HashSet<_>>();

    let mut eliminated_count = 0;
    let mut swapped_labels: HashMap<usize, usize> = Default::default();

    let out_exits: Vec<BasicBlockExit> = vec![];
    let out_basic_blocks: Vec<Vec<(usize, BasicBlockInstruction)>> = vec![];

    let (mut out_exits, out_basic_blocks) = exits.iter().zip(basic_blocks.iter()).enumerate().fold(
        (out_exits, out_basic_blocks),
        |(mut out_exits, mut out_basic_blocks), (i, (exit, block))| {
            let can_eliminate = match out_exits.last() {
                Some(BasicBlockExit::Jump(j)) if *j == i => {
                    !jumped_to.contains(j)
                        && !jumped_to.contains(&(*j + 1))
                        && !jumped_to.contains(&i)
                }
                _ => false,
            };

            match (
                can_eliminate,
                out_exits.last_mut(),
                out_basic_blocks.last_mut(),
            ) {
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
                if let Some(new_j) = swapped_labels.get(j) {
                    *j = *new_j;
                }
            }
            BasicBlockExit::Cond(_, c, a) => {
                if let Some(new_c) = swapped_labels.get(c) {
                    *c = *new_c;
                }
                if let Some(new_a) = swapped_labels.get(a) {
                    *a = *new_a;
                }
            }
            _ => {}
        }
    }

    assert_eq!(out_exits.len(), out_basic_blocks.len());

    (out_exits, out_basic_blocks)
}
