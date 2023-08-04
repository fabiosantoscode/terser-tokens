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
    pub fn get_function(&self, id: FunctionId) -> Option<&BasicBlockGroup> {
        self.functions.get(&id)
    }

    pub fn top_level_stats(&self) -> &BasicBlockGroup {
        &self.functions[&FunctionId(0)]
    }

    pub fn mutate_all_block_groups<Mutator>(&mut self, mutator: &mut Mutator)
    where
        Mutator: FnMut(&mut BasicBlockGroup),
    {
        for (_, function) in self.functions.iter_mut() {
            mutator(function);
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Hash, PartialOrd, Ord, Eq, Default)]
pub struct FunctionId(pub usize);
