use crate::basic_blocks::BasicBlockModule;

use super::{generate_phi_nodes, normalize_basic_blocks, remove_phi_module};

pub fn normalize_module(module: &mut BasicBlockModule) {
    for (_, block_group) in module.iter_mut() {
        let blocks = std::mem::take(&mut block_group.blocks);
        block_group.blocks = normalize_basic_blocks(blocks);
    }

    // TODO: normalize function IDs

    remove_phi_module(module);
    generate_phi_nodes(module);
}
