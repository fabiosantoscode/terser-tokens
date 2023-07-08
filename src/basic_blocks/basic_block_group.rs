use std::fmt::Debug;

use super::basic_block::BasicBlock;

#[derive(Default, Clone)]
pub struct BasicBlockGroup {
    pub blocks: Vec<(usize, BasicBlock)>,
}

impl BasicBlockGroup {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn from_asts(blocks: Vec<BasicBlock>) -> Self {
        Self {
            blocks: blocks
                .into_iter()
                .enumerate()
                .map(|(i, block)| (i, block))
                .collect::<Vec<_>>(),
        }
    }

    pub fn iter<'a>(&'a self) -> core::slice::Iter<'_, (usize, BasicBlock)> {
        self.blocks.iter()
    }
}

impl Debug for BasicBlockGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, v) in self.iter() {
            writeln!(f, "@{}: {:?}", k, v)?;
        }
        Ok(())
    }
}
