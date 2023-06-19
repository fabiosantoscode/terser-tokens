use std::{fmt::Debug, iter::Enumerate};

use super::basic_block::{self, BasicBlock};

#[derive(Default, Clone)]
pub struct BasicBlockGroup {
    pub blocks: Vec<BasicBlock>,
}

impl BasicBlockGroup {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn from_asts(blocks: Vec<BasicBlock>) -> Self {
        Self { blocks }
    }

    pub fn iter<'a>(&'a self) -> Enumerate<std::slice::Iter<'_, basic_block::BasicBlock>> {
        self.blocks.iter().enumerate()
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
