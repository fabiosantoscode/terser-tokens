use super::basic_block_group::{BasicBlockGroup, FunctionId};
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Clone)]
pub struct BasicBlockModule {
    pub summary: ModuleSummary,
    pub top_level_stats: BasicBlockGroup,
    pub functions: HashMap<FunctionId, BasicBlockGroup>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

impl BasicBlockModule {
    pub fn get_function(&self, id: FunctionId) -> Option<&BasicBlockGroup> {
        self.functions.get(&id)
    }
}

impl Debug for BasicBlockModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let BasicBlockModule {
            summary,
            top_level_stats,
            functions,
            imports,
            exports,
        } = self;

        let mut functions = functions.iter().map(|(k, v)| (k.0, v)).collect::<Vec<_>>();
        functions.sort_unstable_by_key(|(k, _)| *k);
        let functions = functions.iter().map(|(_, v)| v).collect::<Vec<_>>();

        let mut d = f.debug_struct("BasicBlockModule");

        d.field("summary", &summary);
        d.field("top_level_stats", &top_level_stats);

        if !functions.is_empty() {
            d.field("functions", &functions);
        }
        if !imports.is_empty() {
            d.field("imports", &self.imports);
        }
        if !exports.is_empty() {
            d.field("exports", &self.exports);
        }

        d.finish()
    }
}
