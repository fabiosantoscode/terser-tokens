use std::collections::{BTreeMap, HashMap};

use crate::basic_blocks::{BasicBlockInstruction, BasicBlockModule};

/// Returns a map of variable indices to instructions that can be inlined.
/// Inlining is when we take advantage of JS's nestable expressions to
/// reduce the amount of variables that need to be emitted.
pub fn get_inlined_variables(
    module: &BasicBlockModule,
    variable_use_count: &BTreeMap<usize, u32>,
) -> BTreeMap<usize, BasicBlockInstruction> {
    let mut inlined_variables = BTreeMap::<usize, BasicBlockInstruction>::new();

    for (_id, block_group) in module.functions.iter() {
        let mut candidates = HashMap::<usize, BasicBlockInstruction>::new();

        for (_id, block) in block_group.blocks.iter() {
            for (var_idx, instruction) in block.instructions.iter() {
                if instruction.can_be_reordered() {
                    if variable_use_count.get(var_idx).unwrap_or(&0) == &1 {
                        candidates.insert(*var_idx, instruction.clone(/* TODO */));
                    }
                } else {
                    // No more inlining allowed, we found a barrier
                    candidates.clear();
                }

                for used_var in instruction.used_vars() {
                    if let Some(inlineable) = candidates.get(used_var) {
                        // Can inline here!
                        inlined_variables.insert(*used_var, inlineable.clone());
                    }
                }
            }

            for used_var_in_exit in block.exit.used_vars() {
                if let Some(candidate) = candidates.get(&used_var_in_exit) {
                    // Can inline here!
                    inlined_variables.insert(used_var_in_exit, candidate.clone());
                }
            }
        }
    }

    inlined_variables
}
