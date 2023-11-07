use std::collections::BTreeMap;

use crate::basic_blocks::{BasicBlock, BasicBlockModule};

/// Count how often a variable is used.
pub fn count_variable_writes(module: &BasicBlockModule) -> BTreeMap<usize, u32> {
    let mut writes: BTreeMap<usize, u32> = BTreeMap::new();

    for (_, _, block) in module.iter_all_blocks() {
        count_block(&mut writes, block);
    }

    writes
}

fn count_block(writes: &mut BTreeMap<usize, u32>, block: &BasicBlock) {
    for (varname, _) in block.iter_all_instructions() {
        writes.entry(varname).and_modify(|e| *e += 1).or_insert(1);
    }
}
