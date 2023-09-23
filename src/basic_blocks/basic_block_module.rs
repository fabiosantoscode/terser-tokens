use std::collections::BTreeMap;

use super::BasicBlockGroup;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ModuleSummary {
    pub filename: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Import {
    Name(String, String),
    Star(String),
    Default(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Export {
    Name(String, String),
    Star(String),
    Default(String),
}

#[derive(Clone, Default)]
pub struct BasicBlockModule {
    pub summary: ModuleSummary,
    pub functions: BTreeMap<FunctionId, BasicBlockGroup>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

impl BasicBlockModule {
    /// Get a function. FunctionId(0) is the top level stats.
    pub fn get_function(&self, id: FunctionId) -> Option<&BasicBlockGroup> {
        self.functions.get(&id)
    }

    /// Get the top level statements of the module.
    pub fn top_level_stats(&self) -> &BasicBlockGroup {
        &self.functions[&FunctionId(0)]
    }

    pub fn iter(&self) -> impl Iterator<Item = (FunctionId, &BasicBlockGroup)> {
        self.functions.iter().map(|(id, function)| (*id, function))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (FunctionId, &mut BasicBlockGroup)> {
        self.functions
            .iter_mut()
            .map(|(id, function)| (*id, function))
    }
}

/// A usize that uniquely points to a function.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Hash, PartialOrd, Ord, Eq, Default)]
pub struct FunctionId(pub usize);
