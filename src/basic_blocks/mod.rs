pub mod basic_block;
pub mod basic_block_group;
pub mod basic_block_module;
pub mod normalize;

#[cfg(test)]
pub mod testutils;

pub use basic_block::{
    ArrayElement, BasicBlock, BasicBlockExit, BasicBlockInstruction, ExitType, TempExitType,
};
pub use basic_block_group::{
    BasicBlockEnvironment, BasicBlockEnvironmentType, BasicBlockGroup, FunctionId,
};
pub use basic_block_module::{BasicBlockModule, Export, Import, ModuleSummary};
pub use normalize::normalize_basic_blocks;
