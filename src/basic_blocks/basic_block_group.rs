use super::BasicBlock;

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
