use std::collections::BTreeMap;

use super::{BasicBlock, FunctionId};

/// Represents a group of basic blocks that are part of the same function or module.
#[derive(Default, Clone)]
pub struct BasicBlockGroup {
    pub id: FunctionId,
    pub blocks: BTreeMap<usize, BasicBlock>,
    pub environment: BasicBlockEnvironment,
}

#[derive(Default, Clone)]
pub enum BasicBlockEnvironment {
    #[default]
    Module,
    /// (is_generator, is_async)
    Function(bool, bool),
}

impl BasicBlockEnvironment {
    pub fn unwrap_function(&self) -> (bool, bool) {
        match self {
            BasicBlockEnvironment::Function(is_generator, is_async) => (*is_generator, *is_async),
            _ => panic!("not a function"),
        }
    }
}

impl BasicBlockGroup {
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (usize, &'a BasicBlock)> {
        self.blocks.iter().map(|(id, block)| (*id, block))
    }

    pub(crate) fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = (usize, &'a mut BasicBlock)> {
        self.blocks.iter_mut().map(|(id, block)| (*id, block))
    }

    /// Get the range of block ids in this group.
    pub fn get_block_range(&self) -> (usize, usize) {
        (
            *(self.blocks.first_key_value().expect("no blocks").0),
            *(self.blocks.last_key_value().expect("no blocks").0),
        )
    }
}
