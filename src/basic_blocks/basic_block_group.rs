use super::{BasicBlock, NonLocalId};

#[derive(Default, Clone)]
pub struct BasicBlockGroup {
    pub blocks: Vec<(usize, BasicBlock)>,
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
    pub fn from_asts(blocks: Vec<BasicBlock>) -> Self {
        Self {
            blocks: blocks.into_iter().enumerate().collect(),
            ..Default::default()
        }
    }

    pub fn iter<'a>(&'a self) -> core::slice::Iter<'_, (usize, BasicBlock)> {
        self.blocks.iter()
    }
}
