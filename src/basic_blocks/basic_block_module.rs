use super::basic_block_group::BasicBlockGroup;

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleSummary {
    pub filename: String,
}

#[derive(Debug, Clone)]
pub struct BasicBlockModule {
    pub summary: ModuleSummary,
    pub top_level_stats: BasicBlockGroup,
}
