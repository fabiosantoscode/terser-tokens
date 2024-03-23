use std::collections::BTreeMap;

use super::{FunctionId, StructuredFlow, StructuredFunction};

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
pub struct StructuredModule {
    pub summary: ModuleSummary,
    pub functions: BTreeMap<FunctionId, StructuredFunction>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

impl StructuredModule {
    /// Get a function. FunctionId(0) is the top level stats.
    pub fn get_function(&self, id: FunctionId) -> Option<&StructuredFunction> {
        self.functions.get(&id)
    }

    /// Get the top level statements of the module.
    pub fn top_level_stats(&self) -> &StructuredFunction {
        &self.functions[&FunctionId(0)]
    }

    /// Remove a function. FunctionId(0) is the top level stats.
    pub fn take_function(&mut self, id: FunctionId) -> Option<StructuredFunction> {
        self.functions.remove(&id)
    }

    /// Get the top level statements of the module.
    pub fn take_top_level_stats(&mut self) -> StructuredFunction {
        self.take_function(FunctionId(0))
            .expect("no top level stats")
    }

    pub fn iter(&self) -> impl Iterator<Item = (FunctionId, &StructuredFunction)> {
        self.functions.iter().map(|(id, function)| (*id, function))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (FunctionId, &mut StructuredFunction)> {
        self.functions
            .iter_mut()
            .map(|(id, function)| (*id, function))
    }

    pub(crate) fn for_each_flow_mut<F>(&mut self, mut cb: F) -> ()
    where
        F: FnMut(FunctionId, &mut StructuredFlow),
    {
        for (func_id, func) in self.functions.iter_mut() {
            func.for_each_flow_mut(|block| cb(*func_id, block));
        }
    }
}
