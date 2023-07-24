use std::fmt::Debug;

use super::basic_block::BasicBlock;

#[derive(Default, Clone)]
pub struct BasicBlockGroup {
    pub blocks: Vec<(usize, BasicBlock)>,
    pub environment: BasicBlockEnvironment,
}

#[derive(Default, Clone)]
pub enum BasicBlockEnvironmentType {
    #[default]
    Module,
    Function(usize),
}

#[derive(Default, Clone)]
pub struct BasicBlockEnvironment {
    pub env_type: BasicBlockEnvironmentType,
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub struct FunctionId(pub usize);

impl BasicBlockGroup {
    pub fn from_asts(blocks: Vec<BasicBlock>) -> Self {
        Self {
            blocks: blocks
                .into_iter()
                .enumerate()
                .map(|(i, block)| (i, block))
                .collect::<Vec<_>>(),
            ..Default::default()
        }
    }

    pub fn iter<'a>(&'a self) -> core::slice::Iter<'_, (usize, BasicBlock)> {
        self.blocks.iter()
    }
}

impl Debug for BasicBlockGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.environment.env_type {
            BasicBlockEnvironmentType::Module => {}
            BasicBlockEnvironmentType::Function(argc) => writeln!(f, "function():")?,
        }
        for (k, v) in self.iter() {
            writeln!(f, "@{}: {:?}", k, v)?;
        }
        Ok(())
    }
}
