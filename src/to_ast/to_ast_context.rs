use std::collections::BTreeMap;

use crate::{
    basic_blocks::{BasicBlockInstruction, BasicBlockModule},
    to_ast::Base54,
};

#[derive(Debug)]
pub struct ToAstContext<'a> {
    pub caught_error: Option<Base54>,
    pub module: &'a BasicBlockModule,
    pub inlined_variables: BTreeMap<usize, BasicBlockInstruction>,
    pub variable_use_count: BTreeMap<usize, u32>,
    pub emitted_vars: BTreeMap<usize, Base54>,
    pub gen_var_index: Base54,
}

impl ToAstContext<'_> {
    pub fn get_caught_error(&mut self) -> String {
        let err = self
            .caught_error
            .take()
            .expect("reference to caught error must be inside catch block");

        err.to_string()
    }

    pub fn set_caught_error(&mut self) -> String {
        let error_index = self.gen_var_index;
        self.gen_var_index = error_index.next();
        self.caught_error = Some(error_index);
        error_index.to_string()
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

    pub(crate) fn get_varname_for(&self, var_idx: usize) -> String {
        let var_idx = *self.emitted_vars.get(&var_idx).unwrap();
        var_idx.to_string()
    }

    pub(crate) fn create_varname_for(&mut self, var_idx: usize) -> String {
        assert!(!self.emitted_vars.contains_key(&var_idx));
        let gen = self.gen_var_index;
        self.gen_var_index = gen.next();
        self.emitted_vars.insert(var_idx, gen);
        gen.to_string()
    }
}
