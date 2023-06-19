// The $0 = either($1 $2 $3) is a phi node, which is a special node that can be either $1, $2, or $3.
// This is no longer useful
// We need to find these and replace $1 $2 $3, whenever they're found, with just $0

use std::collections::{HashMap, HashSet};

use crate::basic_blocks::{
    basic_block::{BasicBlock, BasicBlockInstruction},
    basic_block_group::BasicBlockGroup,
};

pub fn remove_phi(group: &mut BasicBlockGroup) {
    let mut phies_to_final_name: HashMap<usize, usize> = collect_phi(group);

    for block in group.blocks.iter_mut() {
        remove_phi_inner(block, &mut phies_to_final_name);
    }
}

fn collect_phi(group: &BasicBlockGroup) -> HashMap<usize, usize> {
    let mut phies_to_final_name: HashMap<usize, usize> = Default::default();

    for block in group.blocks.iter() {
        for (varname, ins) in block.instructions.iter() {
            if let BasicBlockInstruction::Phi(phies) = ins {
                for phi in phies {
                    phies_to_final_name.insert(*phi, *varname);
                }
            }
        }
    }

    phies_to_final_name
}

fn remove_phi_inner(block: &mut BasicBlock, phies_to_final_name: &mut HashMap<usize, usize>) {
    let mut phies_found: HashSet<usize> = Default::default();

    for (varname, ins) in block.instructions.iter() {
        if let BasicBlockInstruction::Phi(phies) = ins {
            phies_found.insert(*varname);
            for phi in phies {
                phies_to_final_name.insert(*phi, *varname);
            }
        }
    }

    block
        .instructions
        .retain(|(varname, _)| phies_found.contains(varname) == false);

    for x in block.instructions.iter_mut() {
        if let Some(final_name) = phies_to_final_name.get(&x.0) {
            (*x).0 = *final_name;
        }
    }

    for (_, ins) in block.instructions.iter_mut() {
        for used_var in ins.used_vars_mut() {
            if let Some(final_name) = phies_to_final_name.get(used_var) {
                *used_var = *final_name;
                todo!("test this case")
            }
        }
    }
}
