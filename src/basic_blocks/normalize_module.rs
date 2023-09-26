use std::collections::BTreeMap;

use super::{generate_phi_nodes, normalize_basic_blocks, remove_phi_module, BasicBlockModule};

pub fn normalize_module(module: &mut BasicBlockModule) {
    for (_, block_group) in module.iter_mut() {
        let blocks = std::mem::take(&mut block_group.blocks);
        block_group.blocks = normalize_basic_blocks(blocks);
    }

    // TODO: normalize function IDs

    remove_phi_module(module);
    generate_phi_nodes(module); // calls `normalize_varnames`
}

/// make names monotonically increase
pub fn normalize_varnames(module: &mut BasicBlockModule) {
    let mut renamed_vars = BTreeMap::new();
    let mut varname = 0usize;

    for (_, block_group) in module.iter_mut() {
        for (_, block) in block_group.iter_mut() {
            for (original_varname, instruction) in block.instructions.iter_mut() {
                let new_varname = incr(&mut varname);

                // Rename this var and record the rename for future usages
                renamed_vars.insert(*original_varname, new_varname);
                *original_varname = new_varname;

                // Rename the nonlocal variable, if any
                if let Some(nonlocal) = instruction.get_nonlocal_id_mut() {
                    if let Some(new_name) = renamed_vars.get(nonlocal) {
                        *nonlocal = *new_name;
                    } else {
                        let new_varname = incr(&mut varname);

                        renamed_vars.insert(*nonlocal, new_varname);
                        *nonlocal = new_varname;
                    }
                }

                // Rename all variables used by this instruction
                for arg in instruction.used_vars_mut() {
                    if let Some(new_name) = renamed_vars.get(arg) {
                        *arg = *new_name;
                    }
                }
            }

            // Rename all variables used by the exit
            for used_exit_var in block.exit.used_vars_mut() {
                *used_exit_var = renamed_vars[used_exit_var];
            }
        }
    }
}

fn incr(i: &mut usize) -> usize {
    let res = *i;
    *i += 1;
    res
}
