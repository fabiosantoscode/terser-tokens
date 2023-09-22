use std::collections::BTreeMap;

use super::{BasicBlock, FunctionId, NonLocalId};

#[derive(Default, Clone)]
pub struct BasicBlockGroup {
    pub id: FunctionId,
    pub blocks: BTreeMap<usize, BasicBlock>,
    pub environment: BasicBlockEnvironment,
}

#[derive(Default, Clone)]
pub struct BasicBlockEnvironment {
    pub env_type: BasicBlockEnvironmentType,
    pub provided_nonlocals: Vec<NonLocalId>,
    pub used_nonlocals: Vec<NonLocalId>,
}

#[derive(Default, Clone)]
pub enum BasicBlockEnvironmentType {
    #[default]
    Module,
    Function(usize),
}

impl BasicBlockGroup {
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (usize, &'a BasicBlock)> {
        self.blocks.iter().map(|(id, block)| (*id, block))
    }

    pub(crate) fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = (usize, &'a mut BasicBlock)> {
        self.blocks.iter_mut().map(|(id, block)| (*id, block))
    }

    pub fn get_block_range(&self) -> (usize, usize) {
        (
            *(self.blocks.first_key_value().expect("no blocks").0),
            *(self.blocks.last_key_value().expect("no blocks").0),
        )
    }
}
