use super::basic_block_group::{BasicBlockGroup, FunctionId};
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleSummary {
    pub filename: String,
}

#[derive(Clone)]
pub struct BasicBlockModule {
    pub summary: ModuleSummary,
    pub top_level_stats: BasicBlockGroup,
    pub functions: HashMap<FunctionId, BasicBlockGroup>,
}

impl BasicBlockModule {
    pub fn get_function(&self, id: FunctionId) -> Option<&BasicBlockGroup> {
        self.functions.get(&id)
    }
}

impl Debug for BasicBlockModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let mut functions = self
            .functions
            .iter()
            .map(|(k, v)| (k.0, v))
            .collect::<Vec<_>>();
        functions.sort_unstable_by_key(|(k, _)| *k);
        let functions = functions.iter().map(|(k, v)| v).collect::<Vec<_>>();

        f.debug_struct("BasicBlockModule")
            .field("summary", &self.summary)
            .field("top_level_stats", &self.top_level_stats)
            .field("functions", &functions)
            .finish()
    }
}