use std::collections::{BTreeMap, BTreeSet};

use crate::basic_blocks::{BasicBlockInstruction, BasicBlockModule, NonLocalId};

pub struct ToAstContext<'a> {
    pub caught_error: Option<String>,
    pub error_counter: usize,
    pub module: &'a BasicBlockModule,
    pub inlined_variables: BTreeMap<usize, BasicBlockInstruction>,
    pub variable_use_count: BTreeMap<usize, u32>,
    pub emitted_vars: BTreeSet<usize>,
    pub emitted_nonlocals: BTreeSet<NonLocalId>,
}

impl ToAstContext<'_> {
    pub fn get_caught_error(&mut self) -> String {
        self.caught_error
            .take()
            .expect("reference to caught error must be inside catch block")
    }

    pub fn set_caught_error(&mut self) -> String {
        self.error_counter += 1;
        let varname = format!("$error{}", self.error_counter);
        self.caught_error = Some(varname.clone());
        varname
    }

    pub fn will_be_inlined(&self, variable: usize) -> bool {
        self.inlined_variables.contains_key(&variable)
    }

    pub fn get_inlined_expression(&mut self, var_idx: usize) -> Option<BasicBlockInstruction> {
        self.inlined_variables.remove(&var_idx)
    }

    pub fn variable_has_uses(&self, variable: usize) -> bool {
        self.variable_use_count.get(&variable).unwrap_or(&0) > &0
    }
}
